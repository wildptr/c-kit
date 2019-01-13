open ExtLib
open PP_Token
open Token_Conv

module H =
  Hashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end)

module M = Map.Make(String)

type token = {
  kind : pptoken;
  text : string;
  pos : Lexing.position;
  ws : string; (* preceding whitespace *)
  no_expand_list : string list;
}

let eof = {
  kind = EOF;
  text = "";
  pos = Lexing.dummy_pos;
  ws = "";
  no_expand_list = []
}

type replace_token =
  | Verbatim of token
  | Param of int
  | Stringify of string * int
  | Concat of replace_token * replace_token
  | Magic_FILE
  | Magic_LINE

type macro_def =
  | ObjLike of replace_token list
  | FunLike of int * replace_token list

type state = {
  lex_state : Lexer.state;
  mutable lexbuf : Lexing.lexbuf;
  macro_tab : macro_def H.t;
  token_stack : token Stack.t;
  mutable cond_stack : (bool * bool) list;
  sys_include_dirs : string list;
  user_include_dirs : string list;
  mutable input_chan : in_channel;
  mutable include_stack : (Lexing.lexbuf * in_channel) list;
  max_include_depth : int
}

let init_state input_chan =
  let lexbuf = Lexing.from_channel input_chan in
  let macro_tab =
    [
      "__FILE__", ObjLike [Magic_FILE];
      "__LINE__", ObjLike [Magic_LINE]
    ] |> List.to_seq |> H.of_seq
  in
  { lex_state = Lexer.init_state ();
    lexbuf;
    include_stack = [];
    macro_tab;
    token_stack = Stack.create ();
    cond_stack = [];
    sys_include_dirs = ["/usr/local/include/"; "/usr/include/"];
    user_include_dirs = [""];
    input_chan;
    max_include_depth = 16 }

let wrap_token kind lex_state lexbuf =
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_p in
  let ws = Buffer.contents lex_state.Lexer.whitespace in
  Buffer.clear lex_state.Lexer.whitespace;
  { kind; text; pos; ws; no_expand_list = [] }

let before x f = f x; x

let lex_raw lex_state lexbuf =
  let kind = Lexer.token lex_state lexbuf in
  wrap_token kind lex_state lexbuf
(*before (wrap_token kind lex_state lexbuf)
    (fun { kind; pos; ws; text; _ } ->
       Printf.eprintf "[lex_raw] %d:%d: ‘%s’ ‘%s’\n"
         pos.pos_lnum (pos.pos_cnum - pos.pos_bol) ws text)*)

exception Error of string

let error s = raise (Error s)

let pp_token_list f l =
  let open Format in
  l |> List.iter begin fun t ->
    pp_print_string f t.ws;
    pp_print_string f t.text
  end

let parse_macro_arg getsym lookahead =
  let rec loop depth acc t =
    match t.kind with
    | Comma when depth = 0 -> acc, t
    | LParen -> loop (depth+1) (t :: acc) (getsym ())
    | RParen ->
      if depth = 0 then acc, t else
        loop (depth-1) (t :: acc) (getsym ())
    | EOF -> error "unterminated macro argument"
    | _ -> loop depth (t :: acc) (getsym ())
  in
  let l, lookahead' = loop 0 [] lookahead in
  List.rev l, lookahead'

let parse_macro_arg_list getsym lookahead =
  match lookahead.kind with
  | RParen -> []
  | _ ->
    let rec loop lookahead =
      let arg, lookahead' = parse_macro_arg getsym lookahead in
      match lookahead'.kind with
      | RParen -> [arg]
      | Comma -> arg :: loop (getsym ())
      | _ -> error "macro argument list"
    in
    loop lookahead

let parse_token s =
  let lexbuf = Lexing.from_string s in
  let lex_state = Lexer.init_state () in
  let t = Lexer.token lex_state lexbuf in
  if lexbuf.lex_curr_pos < String.length s then
    error (Printf.sprintf "not a valid token: ‘%s’" s);
  t

let quote s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  s |> String.iter begin fun c ->
    if c = '"' || c = '\\' then Buffer.add_char buf '\\';
    Buffer.add_char buf c
  end;
  Buffer.add_char buf '"';
  Buffer.contents buf

let stringify pos ws l =
  let s =
    match l with
    | [] -> ""
    | hd::tl ->
      let buf = Buffer.create 16 in
      Buffer.add_string buf hd.text;
      tl |> List.iter begin fun t ->
        Buffer.add_string buf t.ws;
        Buffer.add_string buf t.text
      end;
      Buffer.contents buf
  in
  { kind = StringLit; text = quote s;
    pos; ws = ""; no_expand_list = [] }

let concat_token pos t1 t2 =
  let text = t1.text ^ t2.text in
  { kind = parse_token text; text;
    pos; ws = ""; no_expand_list = [] }

let rec parse_macro_body param_map = function
  | Verbatim { kind = Ident; text = s; _ } :: tl
    when M.mem s param_map ->
    let i = M.find s param_map in
    parse_macro_body param_map (Param i :: tl)
  | (Verbatim { kind = Hash; ws; _ } as hd1) ::
    (Verbatim { kind = Ident; text = s; _ } as hd2) :: tl ->
    begin match M.find s param_map with
      | i -> Stringify (ws, i) :: parse_macro_body param_map tl
      | exception Not_found -> hd1 :: hd2 :: parse_macro_body param_map tl
    end
  | hd1 :: Verbatim { kind = HashHash; _ } :: tl ->
    let hd', tl' =
      match parse_macro_body param_map tl with
      | h::t -> h, t
      | _ -> error "operand of ## missing"
    in
    Concat (hd1, hd') :: tl'
  | hd :: tl -> hd :: parse_macro_body param_map tl
  | [] -> []

let rec subst_token (macro_name, noexp, pos, ws as token_info) arg_tab = function
  | Verbatim u -> [{ u with no_expand_list = macro_name :: noexp; pos }]
  | Param i ->
    arg_tab.(i) |> List.map
      (fun u -> { u with no_expand_list = macro_name :: u.no_expand_list })
  | Stringify (ws, i) -> [stringify pos ws arg_tab.(i)]
  | Concat (u1, u2) ->
    let u1' = subst_token token_info arg_tab u1 in
    let u2' = subst_token token_info arg_tab u2 in
    begin match u1', u2' with
      | [], _ -> u2'
      | _, [] -> u1'
      | _, t2::l2 ->
        let l1, t1 =
          match List.rev u1' with
          | h::t -> List.rev t, h
          | _ -> assert false
        in
        l1 @ concat_token pos t1 t2 :: l2
    end
  | Magic_FILE ->
    [{ kind = StringLit; text = quote pos.pos_fname; pos; ws; no_expand_list = [] }]
  | Magic_LINE ->
    [{ kind = IntLit; text = string_of_int pos.pos_lnum; pos; ws; no_expand_list = [] }]

let directive_type dir =
  let lex_state = Lexer.init_state () in
  let lexbuf = Lexing.from_string dir in
  let t = lex_raw lex_state lexbuf in
  t.text

let rec find_include_file search_path filename =
  match search_path with
  | [] -> None
  | hd :: tl ->
    let path = hd ^ filename in
    if Sys.file_exists path then Some path else
      find_include_file tl filename

let push_include st path =
  if List.length st.include_stack >= st.max_include_depth then
    error "too many nested #include's";
  let ic = open_in path in
  st.include_stack <- (st.lexbuf, st.input_chan) :: st.include_stack;
  let lexbuf = Lexing.from_channel ic in
  st.lexbuf <- lexbuf;
  st.input_chan <- ic

let pop_include st =
  close_in st.input_chan;
  match st.include_stack with
  | [] -> failwith "pop_include"
  | (lb, ic) :: tl ->
    st.include_stack <- tl;
    st.lexbuf <- lb;
    st.input_chan <- ic

let handle_directive st pos dir =
  let lex_state = Lexer.init_state () in
  let lexbuf = Lexing.from_string dir in
  lexbuf.lex_abs_pos <- pos.Lexing.pos_cnum;
  lexbuf.lex_curr_p <- pos;
  let getsym () = lex_raw lex_state lexbuf in
  let expect_ident () =
    let t = getsym () in
    match t.kind with
    | Ident -> t.text
    | _ -> error "identifier expected"
  in
  let parse_token_list () =
    let rec loop acc =
      let t = getsym () in
      if t.kind = EOF then acc else loop (t::acc)
    in
    loop [] |> List.rev
  in
  let dir_name_token = getsym () in
  if dir_name_token.kind = Ident then
    match dir_name_token.text with
    | "define" ->
      let name = expect_ident () in
      let def =
        match getsym () with
        | { kind = LParen; ws = ""; _ } ->
          (* function-like macro *)
          let param_map, arity =
            match getsym () with
            | { kind = RParen; _ } -> M.empty, 0
            | { kind = Ident; text = s; _ } ->
              let rec loop m i =
                match (getsym ()).kind with
                | Comma ->
                  let s = expect_ident () in
                  loop (M.add s i m) (i+1)
                | RParen -> m, i
                | _ -> error "#define"
              in
              loop (M.singleton s 0) 1
            | _ -> error "#define"
          in
          let body =
            parse_macro_body param_map
              (parse_token_list () |> List.map (fun t -> Verbatim t))
          in
          FunLike (arity, body)
        | t ->
          (* object-like macro *)
          let body =
            parse_macro_body M.empty
              ((t :: parse_token_list ()) |> List.map (fun t -> Verbatim t))
          in
          ObjLike body
      in
      H.replace st.macro_tab name def
    | "undef" ->
      let name = expect_ident () in
      H.remove st.macro_tab name
    | "ifdef" ->
      begin match parse_token_list () with
        | [{ kind = Ident; text = name; _ }] ->
          st.cond_stack <-
            (H.mem st.macro_tab name, false) :: st.cond_stack
        | _ -> error "syntax error in #ifdef"
      end
    | "ifndef" ->
      begin match parse_token_list () with
        | [{ kind = Ident; text = name; _ }] ->
          st.cond_stack <-
            (not (H.mem st.macro_tab name), false) :: st.cond_stack
        | _ -> error "syntax error in #ifndef"
      end
    | "else" ->
      begin match st.cond_stack with
        | [] -> error "#else without #if"
        | (_, true) :: _ -> error "duplicate #else"
        | (cond, false) :: tl -> st.cond_stack <- (not cond, true) :: tl
      end
    | "endif" ->
      begin match st.cond_stack with
        | [] -> error "#endif without #if"
        | _::tl -> st.cond_stack <- tl
      end
    | "include" ->
      let get_filename lexbuf =
        let s = Lexing.lexeme lexbuf in
        String.sub s 1 (String.length s - 2)
      in
      begin match Lexer.include_file lexbuf with
        | SysInclude ->
          let filename = get_filename lexbuf in
          begin match find_include_file st.sys_include_dirs filename with
            | Some path ->
              push_include st path
            | None -> ()
          end
        | UserInclude ->
          let filename = get_filename lexbuf in
          begin match find_include_file st.user_include_dirs filename with
            | Some path ->
              push_include st path
            | None -> ()
          end
      end
    | _ -> error "invalid directive"
  else error "invalid directive"

let at_bol p =
  Lexing.(p.pos_cnum = p.pos_bol)

let rec push_token_list s = function
  | [] -> ()
  | hd :: tl ->
    push_token_list s tl; Stack.push hd s

let subject_to_expansion macro_tab peek t =
  not (List.mem t.text t.no_expand_list) &&
  match H.find macro_tab t.text with
  | ObjLike _ -> true
  | FunLike _ -> (peek ()).kind = LParen
  | exception Not_found -> false

let rec lex_simple macro_tab token_stack =
  let t = Stack.pop token_stack in
  match t.kind with
  | Ident when subject_to_expansion macro_tab
        (fun () -> Stack.top token_stack) t ->
    push_token_list token_stack
      (expand_ident macro_tab (fun () -> Stack.pop token_stack) t);
    lex_simple macro_tab token_stack
  | _ -> t

and expand_ident macro_tab getsym t : token list =
  let s = t.text in
  let expand arg_tab body =
    let l' =
      body |> List.map (subst_token (s, t.no_expand_list, t.pos, t.ws) arg_tab) |> List.concat
    in
    begin match l' with
      | [] -> []
      | hd::tl -> { hd with ws = t.ws } :: tl
    end
  in
  begin match H.find macro_tab s with
    | ObjLike body -> expand [||] body
    | FunLike (arity, body) ->
      let next_token = getsym () in
      assert (next_token.kind = LParen);
      let arg_tab =
        let args = parse_macro_arg_list getsym (getsym ()) in
        let n_arg = List.length args in
        if n_arg = arity then
          args |> List.map (expand_token_list macro_tab) |>
          Array.of_list
        else if n_arg = 0 && arity = 1 then [|[]|]
        else error "wrong number of macro arguments"
      in
      (* arg_tab |> Array.iteri begin fun i l ->
        Format.printf "[%d]=‘%a’@." i pp_token_list l
      end; *)
      expand arg_tab body
  end

and expand_token_list macro_tab l =
  (* Format.printf "expanding ‘%a’@." pp_token_list l; *)
  let s = Stack.create () in
  Stack.push eof s;
  push_token_list s l;
  let rec loop acc =
    match lex_simple macro_tab s with
    | { kind = EOF; _ } -> acc
    | t -> loop (t::acc)
  in
  loop [] |> List.rev
  (* |> fun result ->
  Format.printf "‘%a’ → ‘%a’@." pp_token_list l pp_token_list result;
  result *)

let lex_raw' st =
  if Stack.is_empty st.token_stack then
    lex_raw st.lex_state st.lexbuf
  else Stack.pop st.token_stack

let peek st =
  if Stack.is_empty st.token_stack then
    let t = lex_raw st.lex_state st.lexbuf in
    let () = Stack.push t st.token_stack in
    t
  else Stack.top st.token_stack

let rec lex st =
  let t =
    if Stack.is_empty st.token_stack then begin
      begin match st.cond_stack with
        | [] | (true, _) :: _ ->
          lex_raw st.lex_state st.lexbuf
        | (false, false) :: _ ->
          let rec loop () =
            match Lexer.skip_to_directive st.lexbuf with
            | Directive ->
              begin match Lexing.lexeme st.lexbuf with
                | "else" | "endif" -> ()
                | _ -> loop ()
              end
            | EOF -> ()
            | _ -> assert false
          in
          loop ();
          lex_raw st.lex_state st.lexbuf
        | (false, true) :: _ ->
          let rec loop () =
            match Lexer.skip_to_directive st.lexbuf with
            | Directive ->
              begin match Lexing.lexeme st.lexbuf with
                | "endif" -> ()
                | _ -> loop ()
              end
            | EOF -> ()
            | _ -> assert false
          in
          loop ();
          lex_raw st.lex_state st.lexbuf
      end
    end else Stack.pop st.token_stack
  in
  match t.kind with
  | Hash ->
    if at_bol t.pos then begin
      let dir_pos = st.lexbuf.lex_curr_p in
      let _ = Lexer.directive st.lexbuf in
      let dir = Lexing.lexeme st.lexbuf in
      handle_directive st dir_pos dir;
      Buffer.add_string st.lex_state.Lexer.whitespace t.ws;
      lex st
    end else t
  | Ident when subject_to_expansion st.macro_tab (fun () -> peek st) t ->
    push_token_list st.token_stack
      (expand_ident st.macro_tab (fun () -> lex_raw' st) t);
    lex st
  | EOF ->
    if st.include_stack = [] then t else
      let () = pop_include st in
      let t' = lex st in
      { t' with ws = t.ws ^ t'.ws }
  | _ -> t

let make_supplier ic =
  let st = init_state ic in
  fun () ->
    let token = lex st in
(*     Format.printf "%d: %a ‘%s’\n" token.pos pp_pptoken token.kind token.text; *)
    let start_pos = token.pos in
    let end_pos =
      { start_pos with
        pos_cnum = start_pos.pos_cnum + String.length token.text }
    in
    let token' = convert_token token.kind token.text in
    let line = start_pos.pos_lnum in
    let col = start_pos.pos_cnum - start_pos.pos_bol in
    Format.printf "%d:%d: %a ‘%s’\n"
      line col pp_pptoken token.kind token.text;
    token', start_pos, end_pos

let main () =
  let st = init_state stdin in
  let rec loop () =
    let t = lex st in
    print_string t.ws;
    print_string t.text;
    if t.kind = EOF then () else loop ()
  in
  loop ()
