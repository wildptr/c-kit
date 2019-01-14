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

type cond_state =
  | Before      (* before #if/#elif with true condition *)
  | After       (* after #if/#elif with true condition *)
  | Else        (* after #else *)

type state = {
  lex_state : Lexer.state;
  mutable lexbuf : Lexing.lexbuf;
  macro_tab : macro_def H.t;
  token_stack : token Stack.t;
  (* fst indicates whether in active branch *)
  mutable cond_stack : (bool * cond_state) list;
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

let pp_pos f p =
  let open Lexing in
  Format.fprintf f "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let wrap_token kind lex_state lexbuf =
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_p in
  let ws = Buffer.contents lex_state.Lexer.whitespace in
  Buffer.clear lex_state.Lexer.whitespace;
  (*   Format.eprintf "%a: ‘%s’ ‘%s’@." pp_pos pos ws text; *)
  { kind; text; pos; ws; no_expand_list = [] }

let wrap_token_no_ws kind lexbuf =
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_p in
  { kind; text; pos; ws = ""; no_expand_list = [] }

let lex_raw lex_state lexbuf =
  let kind = Lexer.token lex_state lexbuf in
  wrap_token kind lex_state lexbuf

exception Error of Lexing.position * string

let error pos s = raise (Error (pos, s))

let pp_token_list f l =
  let open Format in
  l |> List.iter begin fun t ->
    pp_print_string f t.ws;
    pp_print_string f t.text
  end

let parse_macro_arg next lookahead =
  let rec loop depth acc t =
    match t.kind with
    | Comma when depth = 0 -> acc, t
    | LParen -> loop (depth+1) (t :: acc) (next ())
    | RParen ->
      if depth = 0 then acc, t else
        loop (depth-1) (t :: acc) (next ())
    | EOF -> error t.pos "unterminated macro argument"
    | _ -> loop depth (t :: acc) (next ())
  in
  let l, lookahead' = loop 0 [] lookahead in
  List.rev l, lookahead'

let parse_macro_arg_list next lookahead =
  match lookahead.kind with
  | RParen -> []
  | _ ->
    let rec loop lookahead =
      let arg, lookahead' = parse_macro_arg next lookahead in
      match lookahead'.kind with
      | RParen -> [arg]
      | Comma -> arg :: loop (next ())
      | _ -> error lookahead'.pos "macro argument list"
    in
    loop lookahead

(* s is guaranteed to be non-empty *)
let parse_token s =
  let lexbuf = Lexing.from_string s in
  let lex_state = Lexer.init_state () in
  let t = Lexer.token lex_state lexbuf in
  if lexbuf.lex_start_pos > 0 ||
     lexbuf.lex_curr_pos < String.length s
  then error Lexing.dummy_pos (Printf.sprintf "not a valid token: ‘%s’" s);
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
  | hd1 :: Verbatim { kind = HashHash; pos; _ } :: tl ->
    let hd', tl' =
      match parse_macro_body param_map tl with
      | h::t -> h, t
      | _ -> error pos "operand of ## missing"
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
    error Lexing.dummy_pos "too many nested #include's";
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

type parser_ = {
  next : unit -> token;
  mutable tok : token
}

let getsym p =
  p.tok <- p.next ()

let parse_int s =
  int_of_string s (* TODO *)

let is_oct_char = function
  | '0'..'7' -> true
  | _ -> false

let oct_char_value = function
  | '0'..'7' as c -> Char.code c - 48
  | _ -> assert false

let parse_escape_seq s i =
  match s.[i] with
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'f' -> '\012'
  | 'v' -> '\011'
  | 'a' -> '\007'
  | '0'..'7' ->
    let n = String.length s in
    let j = ref (i+1) in
    let tmp = ref 0 in
    while !j < n && is_oct_char s.[!j] do
      tmp := !tmp*8 + oct_char_value s.[!j];
      incr j
    done;
    Char.chr !tmp
  | _ -> assert false

let parse_char s =
  let c =
    match s.[1] with
    | '\\' -> parse_escape_seq s 2
    | c -> c
  in
  Char.code c

let parse_atom p =
  match p.tok with
  | { kind = IntLit; text; _ } ->
    parse_int text
  | { kind = CharLit; text; _ } ->
    parse_char text
  | _ -> error p.tok.pos "invalid token"

let is_prefix_op = function
  | Minus | Tilde -> true
  | _ -> false

let eval_prefix_op op value =
  match op with
  | Minus -> -value
  | Tilde -> lnot value
  | _ -> assert false

let eval_binary_op op v1 v2 =
  match op with
  | Star -> v1*v2
  | Slash -> v1/v2 (* TODO: check div by 0 *)
  | Percent -> v1 mod v2 (* TODO: check div by 0 *)
  | Plus -> v1+v2
  | Minus -> v1-v2

  | _ -> assert false

let rec parse_prefix_expr p =
  if is_prefix_op p.tok.kind then begin
    let op = p.tok.kind in
    getsym p;
    let value = parse_prefix_expr p in
    eval_prefix_op op value
  end else parse_atom p

let parse_binary_expr parse_sub_expr op_test p =
  let v1 = parse_sub_expr p in
  let rec loop v1 =
    if op_test p.tok.kind then begin
      let op = p.tok.kind in
      getsym p;
      let v2 = parse_sub_expr p in
      eval_binary_op op v1 v2
    end else v1
  in
  loop v1

let parse_mult_expr = parse_binary_expr parse_prefix_expr
    (function Star | Slash | Percent -> true | _ -> false)
let parse_add_expr = parse_binary_expr parse_mult_expr
    (function Plus | Minus -> true | _ -> false)
let parse_shift_expr = parse_binary_expr parse_add_expr
    (function LtLt | GtGt -> true | _ -> false)
let parse_rel_expr = parse_binary_expr parse_shift_expr
    (function Lt | Gt | LtEq | GtEq -> true | _ -> false)
let parse_eq_expr = parse_binary_expr parse_rel_expr
    (function Eq | BangEq -> true | _ -> false)
let parse_and_expr = parse_binary_expr parse_eq_expr ((=) And)
let parse_xor_expr = parse_binary_expr parse_and_expr ((=) Circ)
let parse_or_expr = parse_binary_expr parse_xor_expr ((=) Pipe)
let parse_log_and_expr = parse_binary_expr parse_or_expr ((=) AndAnd)
let parse_log_or_expr = parse_binary_expr parse_log_and_expr ((=) PipePipe)

let expect p text =
  if p.tok.text = text then getsym p else
    error p.tok.pos (Printf.sprintf "‘%s’ expected" text)

let rec parse_cond_expr p =
  let v1 = parse_log_or_expr p in
  if p.tok.kind = Quest then begin
    getsym p;
    let v2 = parse_cond_expr p in
    expect p ":";
    let v3 = parse_cond_expr p in
    if v1 <> 0 then v2 else v3
  end else v1

let rec push_token_list s = function
  | [] -> ()
  | hd :: tl ->
    push_token_list s tl; Stack.push hd s

let expect_s s text =
  let t = Stack.pop s in
  if t.text <> text then error t.pos (Printf.sprintf "‘%s’ expected" text)

let expect_ident s =
  let t = Stack.pop s in
  if t.kind = Ident then t.text else error t.pos "identifier expected"

let subject_to_expansion macro_tab peek t =
  not (List.mem t.text t.no_expand_list) &&
  match H.find macro_tab t.text with
  | ObjLike _ -> true
  | FunLike _ -> (peek ()).kind = LParen
  | exception Not_found -> false

let at_bol p =
  Lexing.(p.pos_cnum = p.pos_bol)

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

let rec lex_simple macro_tab token_stack =
  let t = Stack.pop token_stack in
  match t.kind with
  | Ident when subject_to_expansion macro_tab
        (fun () -> Stack.top token_stack) t ->
    push_token_list token_stack
      (expand_ident macro_tab (fun () -> Stack.pop token_stack) t);
    lex_simple macro_tab token_stack
  | _ -> t

and expand_ident macro_tab next t : token list =
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
      let next_token = next () in
      assert (next_token.kind = LParen);
      let arg_tab =
        let args = parse_macro_arg_list next (next ()) in
        let n_arg = List.length args in
        if n_arg = arity then
          args |> List.map (expand_token_list macro_tab) |>
          Array.of_list
        else if n_arg = 0 && arity = 1 then [|[]|]
        else error t.pos "wrong number of macro arguments"
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

and expand_cond macro_tab s q =
  match Stack.pop s with
  | { text = "defined"; _ } as t ->
    let name =
      match Stack.pop s with
      | { kind = LParen; _ } ->
        let name = expect_ident s in
        let () = expect_s s ")" in
        name
      | { kind = Ident; text; _ } -> text
      | { pos; _ } -> error pos "syntax error"
    in
    let text = if H.mem macro_tab name then "1" else "0" in
    Queue.push { t with kind = IntLit; text } q;
    expand_cond macro_tab s q
  | { kind = Ident; _ } as t ->
    let () =
      if subject_to_expansion macro_tab (fun () -> Stack.top s) t then begin
        let l = expand_ident macro_tab (fun () -> Stack.pop s) t in
        push_token_list s l
      end else Queue.push { t with kind = IntLit; text = "0" } q
    in
    expand_cond macro_tab s q
  | { kind = EOF; _ } -> ()
  | t ->
    Queue.push t q;
    expand_cond macro_tab s q

and handle_else st pos =
  match st.cond_stack with
  | [] -> error pos "#else without #if"
  | (_, Else) :: _ -> error pos "duplicate #else"
  | (_, Before) :: tl -> st.cond_stack <- (true, Else) :: tl
  | (_, After) :: tl -> st.cond_stack <- (false, Else) :: tl

and handle_endif st pos =
  match st.cond_stack with
  | [] -> error pos "#endif without #if"
  | _::tl -> st.cond_stack <- tl

and handle_directive st pos dir =
(*   Format.eprintf "directive: %s@." dir; *)
  let lex_state = Lexer.init_state () in
  let lexbuf = Lexing.from_string dir in
  lexbuf.lex_abs_pos <- pos.Lexing.pos_cnum;
  lexbuf.lex_curr_p <- pos;
  let next () = lex_raw lex_state lexbuf in
  let expect_ident () =
    let t = next () in
    match t.kind with
    | Ident -> t.text
    | _ -> error t.pos "identifier expected"
  in
  let parse_token_list want_eof =
    let rec loop acc =
      let t = next () in
      if t.kind = EOF then
        if want_eof then t::acc else acc
      else loop (t::acc)
    in
    loop [] |> List.rev
  in
  let dir_name_token = next () in
  let handle_elif () =
    match st.cond_stack with
    | [] -> error dir_name_token.pos "#elif without #if"
    | (_, Else) :: _ -> error dir_name_token.pos "#elif after #else"
    | (true, After) :: tl -> st.cond_stack <- (false, After) :: tl
    | (false, After) :: _ -> ()
    | (_, Before) :: tl ->
      let l = parse_token_list true in
      let s = Stack.create () in
      push_token_list s l;
      let q = Queue.create () in
      expand_cond st.macro_tab s q;
      let p = { next = (fun () -> Queue.pop q); tok = Queue.pop q } in
      let active = parse_cond_expr p <> 0 in
      st.cond_stack <- (active, if active then After else Before) :: tl
  in
  match st.cond_stack with
  | [] | (true, _) :: _ ->
    begin match dir_name_token.text with
      | "define" ->
        let name = expect_ident () in
        let def =
          match next () with
          | { kind = LParen; ws = ""; _ } ->
            (* function-like macro *)
            let param_map, arity =
              match next () with
              | { kind = RParen; _ } -> M.empty, 0
              | { kind = Ident; text = s; _ } ->
                let rec loop m i =
                  match next () with
                  | { kind = Comma; _ } ->
                    let s = expect_ident () in
                    loop (M.add s i m) (i+1)
                  | { kind = RParen; _ } -> m, i
                  | { pos; _ } -> error pos "#define"
                in
                loop (M.singleton s 0) 1
              | { pos; _ } -> error pos "#define"
            in
            let body =
              parse_macro_body param_map
                (parse_token_list false |> List.map (fun t -> Verbatim t))
            in
            FunLike (arity, body)
          | t ->
            (* object-like macro *)
            let body =
              parse_macro_body M.empty
                ((t :: parse_token_list false) |> List.map (fun t -> Verbatim t))
            in
            ObjLike body
        in
        H.replace st.macro_tab name def
      | "undef" ->
        let name = expect_ident () in
        H.remove st.macro_tab name
      | "ifdef" ->
        begin match parse_token_list false with
          | [{ kind = Ident; text = name; _ }] ->
            st.cond_stack <-
              let active = H.mem st.macro_tab name in
              (active, if active then After else Before) :: st.cond_stack
          | _ -> error dir_name_token.pos "syntax error in #ifdef"
        end
      | "ifndef" ->
        begin match parse_token_list false with
          | [{ kind = Ident; text = name; _ }] ->
            st.cond_stack <-
              let active = not (H.mem st.macro_tab name) in
              (active, if active then After else Before) :: st.cond_stack
          | _ -> error dir_name_token.pos "syntax error in #ifndef"
        end
      | "elif" -> handle_elif ()
      | "else" -> handle_else st dir_name_token.pos
      | "endif" -> handle_endif st dir_name_token.pos
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
              | None ->
                error lexbuf.lex_start_p ("cannot find <" ^ filename ^ ">")
            end
          | UserInclude ->
            let filename = get_filename lexbuf in
            begin match find_include_file st.user_include_dirs filename with
              | Some path ->
                push_include st path
              | None ->
                error lexbuf.lex_start_p ("cannot find \"" ^ filename ^ "\"")
            end
        end
      | "if" ->
        let l = parse_token_list true in
        let s = Stack.create () in
        push_token_list s l;
        let q = Queue.create () in
        expand_cond st.macro_tab s q;
(*         Format.eprintf "%a@." pp_token_list (Queue.to_seq q |> List.of_seq) *)
        let p = { next = (fun () -> Queue.pop q); tok = Queue.pop q } in
        let active = parse_cond_expr p <> 0 in
        st.cond_stack <- (active, if active then After else Before) :: st.cond_stack
      | "" -> () (* null directive *)
      | _ -> error dir_name_token.pos "invalid directive"
    end
  | (false, _) :: _ ->
    begin match dir_name_token.text with
      | "elif" -> handle_elif ()
      | "else" -> handle_else st dir_name_token.pos
      | "endif" -> handle_endif st dir_name_token.pos
      | _ -> ()
(*       | d -> Format.eprintf "skipping directive %s@." d *)
    end

and lex expand st =
  let t =
    if Stack.is_empty st.token_stack then begin
      begin match st.cond_stack with
        | [] | (true, _) :: _ ->
          lex_raw st.lex_state st.lexbuf
        | (false, _) :: _ ->
          if Lexer.skip_to_directive st.lexbuf then
            wrap_token_no_ws Hash st.lexbuf
          else
            error st.lexbuf.lex_curr_p "unterminated #if"
      end
    end else Stack.pop st.token_stack
  in
  match t.kind with
  | Hash ->
    if at_bol t.pos then begin
      let dir_pos = st.lexbuf.lex_curr_p in
      let _ = Lexer.directive st.lexbuf in
      let dir = Lexing.lexeme st.lexbuf in
      assert (st.lexbuf.lex_curr_p.pos_cnum = st.lexbuf.lex_curr_p.pos_bol);
      begin
        try
          handle_directive st dir_pos dir
        with Error (pos, msg) ->
          Format.eprintf "%a: %s@." pp_pos pos msg;
      end;
      Buffer.add_string st.lex_state.Lexer.whitespace t.ws;
      lex expand st
    end else t
  | Ident when expand && subject_to_expansion st.macro_tab (fun () -> peek st) t ->
    push_token_list st.token_stack
      (expand_ident st.macro_tab (fun () -> lex false st) t);
    lex true st
  | EOF ->
    if st.include_stack = [] then t else
      let () = pop_include st in
      let t' = lex expand st in
      { t' with ws = t.ws ^ t'.ws }
  | _ -> t

let make_supplier ic =
  let st = init_state ic in
  fun () ->
    let token = lex true st in
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
    let t = lex true st in
    print_string t.ws;
    print_string t.text;
    if t.kind = EOF then () else loop ()
  in
  loop ()
