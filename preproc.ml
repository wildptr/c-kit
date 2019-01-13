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
  macro_tab : macro_def H.t;
  mutable token_queue : token list;
  mutable cond_stack : (bool * bool) list
}

let init_state () =
  let macro_tab =
    [
      "__FILE__", ObjLike [Magic_FILE];
      "__LINE__", ObjLike [Magic_LINE]
    ] |> List.to_seq |> H.of_seq
  in
  { lex_state = Lexer.init_state ();
    macro_tab;
    token_queue = [];
    cond_stack = [] }

let wrap_token kind lex_state lexbuf =
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_p in
  let ws = Buffer.contents lex_state.Lexer.whitespace in
  Buffer.clear lex_state.Lexer.whitespace;
  { kind; text; pos; ws; no_expand_list = [] }

let before x f = f x; x

let lex_raw lex_state lexbuf =
  let kind = Lexer.initial lex_state lexbuf in
  wrap_token kind lex_state lexbuf
(*before (wrap_token kind lex_state lexbuf)
    (fun { kind; pos; ws; text; _ } ->
       Printf.eprintf "[lex_raw] %d:%d: ‘%s’ ‘%s’\n"
         pos.pos_lnum (pos.pos_cnum - pos.pos_bol) ws text)*)

let peek (pp_state, lexbuf) =
  let t =
    match pp_state.token_queue with
    | [] ->
      let t = lex_raw pp_state.lex_state lexbuf in
      pp_state.token_queue <- [t]; t
    | t::_ -> t
  in t.kind

let skip (pp_state, _) =
  pp_state.token_queue <- List.tl pp_state.token_queue

let get (pp_state, lexbuf) =
  match pp_state.token_queue with
  | [] -> lex_raw pp_state.lex_state lexbuf
  | hd::tl -> pp_state.token_queue <- tl; hd

exception Error of string

let error s = raise (Error s)

let parse_macro_arg st =
  let rec loop depth acc =
    match peek st with
    | Comma -> if depth = 0 then acc else loop depth (get st :: acc)
    | LParen -> loop (depth+1) (get st :: acc)
    | RParen -> if depth = 0 then acc else loop (depth-1) (get st :: acc)
    | EOF -> error "unterminated macro argument"
    | _ -> loop depth (get st :: acc)
  in
  loop 0 [] |> List.rev

let parse_macro_arg_list st =
  match peek st with
  | RParen -> skip st; []
  | _ ->
    let rec loop acc =
      let acc' = parse_macro_arg st :: acc in
      match peek st with
      | RParen -> skip st; acc'
      | Comma -> skip st; loop acc'
      | _ -> error "macro argument list"
    in
    loop [] |> List.rev

let parse_token s =
  let lexbuf = Lexing.from_string s in
  let lex_state = Lexer.init_state () in
  let t = Lexer.initial lex_state lexbuf in
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

let pp_token_list f l =
  let open Format in
  l |> List.iter begin fun t ->
    pp_print_string f t.ws;
    pp_print_string f t.text
  end

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

let handle_directive pp_state pos dir =
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
      H.replace pp_state.macro_tab name def
    | "undef" ->
      let name = expect_ident () in
      H.remove pp_state.macro_tab name
    | "ifdef" ->
      begin match parse_token_list () with
        | [{ kind = Ident; text = name; _ }] ->
          pp_state.cond_stack <-
            (H.mem pp_state.macro_tab name, false) :: pp_state.cond_stack
        | _ -> error "syntax error in #ifdef"
      end
    | "ifndef" ->
      begin match parse_token_list () with
        | [{ kind = Ident; text = name; _ }] ->
          pp_state.cond_stack <-
            (not (H.mem pp_state.macro_tab name), false) :: pp_state.cond_stack
        | _ -> error "syntax error in #ifndef"
      end
    | "else" ->
      begin match pp_state.cond_stack with
        | [] -> error "#else without #if"
        | (_, true) :: _ -> error "duplicate #else"
        | (cond, false) :: tl -> pp_state.cond_stack <- (not cond, true) :: tl
      end
    | "endif" ->
      begin match pp_state.cond_stack with
        | [] -> error "#endif without #if"
        | _::tl -> pp_state.cond_stack <- tl
      end
    | _ -> error "invalid directive"
  else error "invalid directive"

let rec lex (pp_state, lexbuf as st) =
  let t =
    match pp_state.token_queue with
    | [] ->
      begin match pp_state.cond_stack with
        | [] | (true, _) :: _ ->
          lex_raw pp_state.lex_state lexbuf
        | (false, false) :: _ ->
          let rec loop () =
            match Lexer.skip_to_directive lexbuf with
            | Directive ->
              begin match Lexing.lexeme lexbuf with
                | "else" | "endif" -> ()
                | _ -> loop ()
              end
            | EOF -> ()
            | _ -> assert false
          in
          loop ();
          lex_raw pp_state.lex_state lexbuf
        | (false, true) :: _ ->
          let rec loop () =
            match Lexer.skip_to_directive lexbuf with
            | Directive ->
              begin match Lexing.lexeme lexbuf with
                | "endif" -> ()
                | _ -> loop ()
              end
            | EOF -> ()
            | _ -> assert false
          in
          loop ();
          lex_raw pp_state.lex_state lexbuf
      end
    | hd :: tl ->
      let () = pp_state.token_queue <- tl in hd
  in
  match t.kind with
  | Directive ->
    handle_directive pp_state t.pos t.text;
    Buffer.add_string pp_state.lex_state.Lexer.whitespace t.ws;
    lex st
  | Ident when
      not (List.mem t.text t.no_expand_list) &&
      H.mem pp_state.macro_tab t.text ->
    pp_state.token_queue <- expand_ident st t;
    lex st
  | _ -> t

and expand_ident (pp_state, lexbuf as st) t =
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
  begin match H.find pp_state.macro_tab s with
    | ObjLike body -> expand [||] body
    | FunLike (arity, body) ->
      let next_token = lex_raw pp_state.lex_state lexbuf in
      if next_token.kind = LParen then
        let arg_tab =
          let args = parse_macro_arg_list st in
          let n_arg = List.length args in
          if n_arg = arity then
            args |> List.map (expand_token_list pp_state.macro_tab) |>
            Array.of_list
          else if n_arg = 0 && arity = 1 then [|[]|]
          else error "wrong number of macro arguments"
        in
        (* arg_tab |> Array.iteri begin fun i l ->
           Format.printf "[%d]=‘%a’@." i pp_token_list l
           end; *)
        expand arg_tab body
      else [t; next_token]
  end

and expand_token_list macro_tab l =
  (* Format.printf "expanding ‘%a’@." pp_token_list l; *)
  let lexbuf = Lexing.from_string "" in
  let pp_state = {
    lex_state = Lexer.init_state ();
    macro_tab;
    token_queue = l;
    cond_stack = []
  } in
  let rec loop acc =
    let t = lex (pp_state, lexbuf) in
    if t.kind = EOF then acc else loop (t::acc)
  in
  loop [] |> List.rev
  (* |> fun result ->
  Format.printf "‘%a’ → ‘%a’@." pp_token_list l pp_token_list result;
  result *)

let make_lexer () =
  let pp_state = init_state () in
  fun lexbuf -> lex (pp_state, lexbuf)

let make_supplier ic =
  let lexbuf = Lexing.from_channel ic in
  let pp_state = init_state () in
  fun () ->
    let token = lex (pp_state, lexbuf) in
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
  let lexbuf = Lexing.from_channel stdin in
  let pp_state = init_state () in
  let rec loop () =
    let t = lex (pp_state, lexbuf) in
    print_string t.ws;
    print_string t.text;
    if t.kind = EOF then () else loop ()
  in
  loop ()
