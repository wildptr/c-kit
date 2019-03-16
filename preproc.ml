open Token
open AST_Types

module H =
  Hashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end)

type token' = {
  kind : token;
  text : string;
  pos : Lexing.position;
  ws : string (* preceding whitespace *)
}

type replace_token =
  | Verbatim of token'
  | Param of int
  | Stringify of string * int
  | Concat of replace_token * replace_token
  | Magic_FILE
  | Magic_LINE

type macro_def =
  | ObjLike of replace_token list
  | FunLike of int * bool * replace_token list

type cond_state =
  | Before      (* before true branch *)
  | After       (* after true branch, or nested within a false branch *)
  | Else        (* after #else *)

type cond_stack = (bool * cond_state) list

type state = {
  ws_buf : Buffer.t;
  mutable lexbuf : Lexing.lexbuf;
  macro_tab : macro_def H.t;
  (* fst indicates whether in active branch *)
  mutable cond_stack : cond_stack;
  sys_include_dirs : string list;
  user_include_dirs : string list;
  mutable input_chan : in_channel;
  mutable include_stack : (Lexing.lexbuf * in_channel * cond_stack) list;
  max_include_depth : int
}

let init_state input_chan filename =
  let lexbuf = Lexing.from_channel input_chan in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with
      pos_fname = filename };
  let macro_tab =
    let r kind text =
      Verbatim { kind; text; ws = ""; pos = Lexing.dummy_pos }
    in
    [
      "__FILE__", ObjLike [Magic_FILE];
      "__LINE__", ObjLike [Magic_LINE];
      "__STDC__", ObjLike [r (TInt ("1", true, Size_Int)) "1"];
      "__STDC_VERSION__", ObjLike [r (TInt ("199901", true, Size_Long)) "199901L"];
    ] |> List.to_seq |> H.of_seq
  in
  { ws_buf = Buffer.create 1024;
    lexbuf;
    include_stack = [];
    macro_tab;
    cond_stack = [];
    sys_include_dirs = ["/usr/local/include/"; "/usr/include/"];
    user_include_dirs = [""];
    input_chan;
    max_include_depth = 16 }

let pp_pos f p =
  let open Lexing in
  Format.fprintf f "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let wrap_token_no_ws kind lexbuf =
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_p in
  { kind; text; pos; ws = "" }

let at_bol p ws =
  String.length ws >= Lexing.(p.pos_cnum - p.pos_bol)

(*match offset_from_bol ws with
  | Some n ->
    n = Lexing.(p.pos_cnum - p.pos_bol)
  | None ->
    p.Lexing.pos_cnum = 0*)

let flush_buffer b =
  let s = Buffer.contents b in
  Buffer.clear b;
  s

let lex ws_buf lexbuf =
  let kind = Lexer.token ws_buf lexbuf in
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_p in
  let ws0 = Buffer.contents ws_buf in
(*if kind = Hash then
    Format.eprintf "%a: ‘%s%s’ %b@." pp_pos pos ws0 text (at_bol pos ws0);*)
  if kind = Hash && at_bol pos ws0 then
    { kind = Directive; text; pos; ws = "" }
  else if kind = EOF then
    { kind; text; pos; ws = "" }
  else
    let () = Buffer.clear ws_buf in
    { kind; text; pos; ws = ws0 }

exception Error of Lexing.position * string

let error pos s = raise (Error (pos, s))

let report_error pos msg =
  Format.eprintf "%a: %s@." pp_pos pos msg

let unexpected_token pos = error pos "unexpected token"

let pp_token_list f l =
  let open Format in
  l |> List.iter begin fun t ->
    pp_print_string f t.ws;
    pp_print_string f t.text
  end

(* token stream *)
type parser = {
  next : unit -> token';
  mutable tok : token';
  mutable tokq : token' list
}

let skip p =
  match p.tokq with
  | [] -> p.tok <- p.next ()
  | hd :: tl ->
    p.tokq <- tl;
    p.tok <- hd

let getsym p =
  let t = p.tok in
  skip p;
  t

let ungetsym p t =
  p.tokq <- p.tok :: p.tokq;
  p.tok <- t

let expect p text =
  if p.tok.text = text then skip p else
    error p.tok.pos (Printf.sprintf "‘%s’ expected" text)

let expect_ident p =
  let t = getsym p in
  match t.kind with
  | PreIdent _ -> t.text
  | _ -> error t.pos "identifier expected"

let expect_eof p =
  if p.tok.kind <> EOF then
    error p.tok.pos "extra token"

let parse_macro_arg stop_at_comma p =
  let rec loop depth acc =
    let t = p.tok in
    match t.kind with
    | Comma when stop_at_comma && depth = 0 -> acc
    | LParen -> skip p; loop (depth+1) (t :: acc)
    | RParen ->
      if depth = 0 then acc else begin
        skip p;
        loop (depth-1) (t :: acc)
      end
    | EOF -> error t.pos "unterminated macro argument"
    | _ -> skip p; loop depth (t :: acc)
  in
  let l = loop 0 [] in
  List.rev l

(* return value in reverse order *)
let parse_macro_arg_list arity p =
  skip p; (* '(' *)
  match p.tok.kind with
  | RParen -> skip p; []
  | _ ->
    let rec loop n acc =
      let arg = parse_macro_arg (n < arity) p in
      match getsym p with
      | { kind = RParen; _ } -> arg :: acc
      | { kind = Comma; _ } -> loop (n+1) (arg :: acc)
      | { pos; _ } -> error pos "unexpected_token in macro argument list"
    in
    loop 0 []

let lexbuf_of_string (pos : Lexing.position) s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_abs_pos <- pos.pos_cnum;
  lexbuf.lex_curr_p <- pos;
  lexbuf

(* s is guaranteed to be non-empty *)
let parse_token pos s =
  let lexbuf = Lexing.from_string s in
  let ws_buf = Buffer.create 0 in
  let t = Lexer.token ws_buf lexbuf in
  if lexbuf.lex_start_p.pos_cnum > 0 ||
     lexbuf.lex_curr_p.pos_cnum < String.length s
  then error pos (Printf.sprintf "not a valid token: ‘%s’" s);
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
      (* whitespace before first token is deleted *)
      Buffer.add_string buf hd.text;
      tl |> List.iter begin fun t ->
        if t.ws <> "" then Buffer.add_char buf ' ';
        Buffer.add_string buf t.text
      end;
      Buffer.contents buf
  in
  { kind = TString s; text = quote s; pos; ws }

let get_noexp_list = function
  | { kind = PreIdent l; _ } -> l
  | _ -> []

let concat_token pos t1 t2 =
  let text = t1.text ^ t2.text in
  let kind =
    match parse_token pos text with
    | PreIdent _ -> PreIdent (get_noexp_list t1 @ get_noexp_list t2)
    | k -> k
  in
  { kind; text; pos; ws = "" }

let mark_token macro_name = function
  | { kind = PreIdent l; _ } as t ->
    { t with kind = PreIdent (macro_name :: l) }
  | t -> t

let rec subst_token (macro_name, noexp, pos, ws as token_info)
    arg_tab verbatim = function
  | Verbatim ({ kind = PreIdent _; _ } as t) ->
    [{ t with kind = PreIdent (macro_name :: noexp); pos }]
  | Verbatim t ->
    [{ t with pos }]
  | Param i ->
    (if verbatim then fst else snd) arg_tab.(i) |>
    List.map (mark_token macro_name)
  | Stringify (ws, i) ->
    [stringify pos ws (fst arg_tab.(i))]
  | Concat (u1, u2) ->
    let u1' = subst_token token_info arg_tab true u1 in
    let u2' = subst_token token_info arg_tab true u2 in
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
    let s = pos.pos_fname in
    [{ kind = TString s; text = quote s; pos; ws }]
  | Magic_LINE ->
    let s = string_of_int pos.pos_lnum in
    [{ kind = TInt (s, true, Size_Int); text = s; pos; ws }]

let rec find_include_file search_path filename =
  match search_path with
  | [] -> None
  | hd :: tl ->
    let path = hd ^ filename in
    if Sys.file_exists path then Some path else
      find_include_file tl filename

let push_include st path =
  let ic = open_in path in
  st.include_stack <-
    (st.lexbuf, st.input_chan, st.cond_stack) :: st.include_stack;
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with
      pos_fname = path };
  st.lexbuf <- lexbuf;
  st.input_chan <- ic;
  st.cond_stack <- []

let pop_include st =
  close_in st.input_chan;
  match st.include_stack with
  | [] -> assert false
  | (lb, ic, cs) :: tl ->
    st.include_stack <- tl;
    st.lexbuf <- lb;
    st.input_chan <- ic;
    st.cond_stack <- cs

let parse_int s =
  let n = String.length s in
  let tmp = ref 0L in
  if s.[0] = '0' then begin
    (* octal *)
    let i = ref 1 in
    while !i < n && Lexer.is_oct_digit s.[!i] do
      tmp := Int64.add (Int64.shift_left !tmp 3)
          (Int64.of_int (Lexer.digit_value s.[!i]));
      incr i
    done
  end else begin
    let i = ref 0 in
    while !i < n && Lexer.is_digit s.[!i] do
      tmp := Int64.add (Int64.mul !tmp 10L)
        (Int64.of_int (Lexer.digit_value s.[!i]));
      incr i
    done
  end;
  !tmp

let parse_char s =
  let c =
    match s.[1] with
    | '\\' -> Lexer.parse_escape_seq s (ref 2)
    | c -> c
  in
  Char.code c

let is_prefix_op = function
  | TMinus | Tilde | Bang -> true
  | _ -> false

let eval_prefix_op op v =
  match op with
  | TMinus -> Int64.neg v
  | Tilde -> Int64.lognot v
  | Bang -> if v=0L then 1L else 0L
  | _ -> assert false

let eval_binary_op op v1 v2 =
  match op.kind with
  | Star -> Int64.mul v1 v2
  | Slash ->
    if v2 = 0L then error op.pos "division by 0" else Int64.div v1 v2
  | Percent ->
    if v2 = 0L then error op.pos "division by 0" else Int64.rem v1 v2
  | TPlus -> Int64.add v1 v2
  | TMinus -> Int64.sub v1 v2
  | LtLt -> Int64.shift_left v1 (Int64.to_int v2)
  | GtGt -> Int64.shift_right v1 (Int64.to_int v2)
  | Lt -> if v1<v2 then 1L else 0L
  | Gt -> if v1>v2 then 1L else 0L
  | LtEq -> if v1 <= v2 then 1L else 0L
  | GtEq -> if v1 >= v2 then 1L else 0L
  | EqEq -> if v1 = v2 then 1L else 0L
  | BangEq -> if v1 <> v2 then 1L else 0L
  | TAnd -> Int64.logand v1 v2
  | Circ -> Int64.logxor v1 v2
  | Pipe -> Int64.logor v1 v2
  | AndAnd -> if v1<>0L && v2<>0L then 1L else 0L
  | PipePipe -> if v1<>0L || v2<>0L then 1L else 0L
  | _ -> assert false

let parse_binary_expr parse_sub_expr op_test p =
  let rec loop v1 =
    if op_test p.tok.kind then begin
      let op = p.tok in
      skip p;
      let v2 = parse_sub_expr p in
      loop (eval_binary_op op v1 v2)
    end else v1
  in
  loop (parse_sub_expr p)

let rec parse_atom p =
  match getsym p with
  | { kind = TInt _; text; _ } ->
    parse_int text
  | { kind = TChar _; text; _ } ->
    Int64.of_int (parse_char text)
  | { kind = LParen; _ } ->
    let v = parse_cond_expr p in
    expect p ")";
    v
  | { pos; _ } ->
    unexpected_token pos

and parse_prefix_expr p =
  if is_prefix_op p.tok.kind then begin
    let op = p.tok.kind in
    skip p;
    let value = parse_prefix_expr p in
    eval_prefix_op op value
  end else parse_atom p

and parse_mult_expr p = parse_binary_expr parse_prefix_expr
    (function Star | Slash | Percent -> true | _ -> false) p
and parse_add_expr p = parse_binary_expr parse_mult_expr
    (function TPlus | TMinus -> true | _ -> false) p
and parse_shift_expr p = parse_binary_expr parse_add_expr
    (function LtLt | GtGt -> true | _ -> false) p
and parse_rel_expr p = parse_binary_expr parse_shift_expr
    (function Lt | Gt | LtEq | GtEq -> true | _ -> false) p
and parse_eq_expr p = parse_binary_expr parse_rel_expr
    (function EqEq | BangEq -> true | _ -> false) p
and parse_and_expr p = parse_binary_expr parse_eq_expr ((=) TAnd) p
and parse_xor_expr p = parse_binary_expr parse_and_expr ((=) Circ) p
and parse_or_expr p = parse_binary_expr parse_xor_expr ((=) Pipe) p
and parse_log_and_expr p = parse_binary_expr parse_or_expr ((=) AndAnd) p
and parse_log_or_expr p = parse_binary_expr parse_log_and_expr ((=) PipePipe) p

and parse_cond_expr p =
  let v1 = parse_log_or_expr p in
  if p.tok.kind = Quest then begin
    skip p;
    let v2 = parse_cond_expr p in
    expect p ":";
    let v3 = parse_cond_expr p in
    if v1 <> 0L then v2 else v3
  end else v1

let parse_cond p =
  let v = parse_cond_expr p in
  expect_eof p;
  v

let rec unget_token_list p = function
  | [] -> ()
  | hd :: tl ->
    unget_token_list p tl; ungetsym p hd

let subject_to_expansion macro_tab p text noexp =
  not (List.mem text noexp) &&
  match H.find macro_tab text with
  | ObjLike _ -> true
  | FunLike _ -> p.tok.kind = LParen
  | exception Not_found -> false

let make_parser next =
  { next; tok = next (); tokq = [] }

let rec parse_macro_body param_alist p =
  let parse_simple () =
    match getsym p with
    | { kind = PreIdent _; text = name; _ } as t ->
      begin match List.assoc name param_alist with
        | i -> Param i
        | exception Not_found -> Verbatim t
      end
    | { kind = Hash; ws; _ } as hash ->
      (* According to the standard, each '#' in a function-like macro must be
         followed by a parameter. Here we don't enforce this constraint. *)
      begin match p.tok.kind with
        | PreIdent _ ->
          let name = p.tok.text in
          begin match List.assoc name param_alist with
            | i -> skip p; Stringify (ws, i)
            | exception Not_found -> Verbatim hash
          end
        | _ -> Verbatim p.tok
      end
    | t -> assert (t.kind <> EOF); Verbatim t
  in
  let parse_binary () =
    let rec loop rt1 =
      if p.tok.kind = HashHash then
        let () = skip p in
        let rt2 = parse_simple () in
        loop (Concat (rt1, rt2))
      else rt1
    in
    loop (parse_simple ())
  in
  let rec loop acc =
    if p.tok.kind = EOF then acc else
      loop (parse_binary () :: acc)
  in
  loop [] |> List.rev

let rec getsym_expand macro_tab p =
  let t = getsym p in
  match t.kind with
  | PreIdent noexp when subject_to_expansion macro_tab p t.text noexp ->
    expand_ident macro_tab p t;
    getsym_expand macro_tab p
  | _ -> t

and expand_ident macro_tab p t : unit =
  let noexp = match t.kind with PreIdent l -> l | _ -> assert false in
  let s = t.text in
  let expand arg_tab body =
    let l' =
      body |> List.map (subst_token (s, noexp, t.pos, t.ws) arg_tab false) |> List.concat
    in
    begin match l' with
      | [] -> []
      | hd::tl -> { hd with ws = t.ws } :: tl
    end
  in
  let arg_tab, body =
    match H.find macro_tab s with
    | ObjLike body -> [||], body
    | FunLike (arity, is_vararg, body) ->
      assert (p.tok.kind = LParen);
      (* length of arg_tab should be arity+1 *)
      let args = parse_macro_arg_list arity p in
      let n_arg = List.length args in
      let args' =
        let rec fill_missing_args args n =
          if n <= 0 then args else fill_missing_args ([]::args) (n-1)
        in
        assert (n_arg <= arity+1);
        if is_vararg then begin
          if n_arg = arity + 1 then args else
            fill_missing_args args (arity + 1 - n_arg)
            (*               error t.pos "wrong number of macro arguments" *)
        end else begin
          if n_arg = arity then args else
            fill_missing_args args (arity - n_arg)
            (*               error t.pos "wrong number of macro arguments" *)
        end
      in
      let arg_tab =
        args' |> List.map (fun l -> l, expand_token_list macro_tab l) |>
        List.rev |> Array.of_list
      in
      arg_tab, body
  in
  unget_token_list p (expand arg_tab body)

and expand_token_list macro_tab tok_list =
  (* Format.printf "expanding ‘%a’@." pp_token_list tok_list; *)
  let eof = {
    kind = EOF;
    text = "";
    pos = Lexing.dummy_pos;
    ws = ""
  } in
  let p = make_parser (fun () -> eof) in
  unget_token_list p tok_list;
  let rec loop acc =
    match getsym_expand macro_tab p with
    | { kind = EOF; _ } -> acc
    | t -> loop (t::acc)
  in
  loop [] |> List.rev
(* |> fun result ->
   Format.printf "‘%a’ → ‘%a’@." pp_token_list tok_list pp_token_list result;
   result *)

let make_cond_expander macro_tab p =
  make_parser begin fun () ->
    match getsym_expand macro_tab p with
    | { text = "defined"; _ } as t ->
      let name =
        match getsym p with
        | { kind = LParen; _ } ->
          let name = expect_ident p in
          let () = expect p ")" in
          name
        | { kind = PreIdent _; text; _ } -> text
        | { pos; _ } -> error pos "identifier expected after ‘defined’"
      in
      let text = if H.mem macro_tab name then "1" else "0" in
      { t with kind = TInt (text, true, Size_Int); text }
    | { kind = PreIdent _ } as t ->
      { t with kind = TInt ("0", true, Size_Int); text = "0" }
    | t -> t
  end

let handle_else st p =
  let pos = p.tok.pos in
  skip p;
  expect_eof p;
  match st.cond_stack with
  | [] -> error pos "#else without #if"
  | (_, Else) :: _ -> error pos "duplicate #else"
  | (_, Before) :: tl -> st.cond_stack <- (true, Else) :: tl
  | (_, After) :: tl -> st.cond_stack <- (false, Else) :: tl

let handle_endif st p =
  let pos = p.tok.pos in
  skip p;
  expect_eof p;
  match st.cond_stack with
  | [] -> error pos "#endif without #if"
  | _::tl ->
    st.cond_stack <- tl

let handle_elif st p =
  let pos = p.tok.pos in
  skip p;
  match st.cond_stack with
  | [] -> error pos "#elif without #if"
  | (_, Else) :: _ -> error pos "#elif after #else"
  | (true, After) :: tl -> st.cond_stack <- (false, After) :: tl
  | (false, After) :: _ -> ()
  | (_, Before) :: tl ->
    let active = parse_cond (make_cond_expander st.macro_tab p) <> 0L in
    st.cond_stack <- (active, if active then After else Before) :: tl

let handle_define st p =
  skip p;
  let name = expect_ident p in
  let def =
    match p.tok with
    | { kind = LParen; ws = ""; _ } ->
      skip p;
      (* function-like macro *)
      let param_alist, arity, is_vararg =
        match getsym p with
        | { kind = RParen; _ } -> [], 0, false
        | { kind = PreIdent _; text; _ } ->
          let rec loop m i =
            match getsym p with
            | { kind = Comma; _ } ->
              begin match getsym p with
                | { kind = PreIdent _; text; _ } ->
                  loop ((text, i) :: m) (i+1)
                | { kind = Ellipsis; _ } ->
                  expect p ")";
                  m, i, true
                | { pos; _ } -> unexpected_token pos
              end
            | { kind = RParen; _ } -> m, i, false
            | { pos; _ } -> unexpected_token pos
          in
          loop [text, 0] 1
        | { kind = Ellipsis; _ } ->
          expect p ")";
          [], 0, true
        | { pos; _ } -> unexpected_token pos
      in
      let param_alist' =
        List.rev (* reverse so __VA_ARGS__ comes last *)
          (if is_vararg then
             ("__VA_ARGS__", arity) :: param_alist
           else param_alist)
      in
      let body = parse_macro_body param_alist' p in
      FunLike (arity, is_vararg, body)
    | t ->
      (* object-like macro *)
      let body = parse_macro_body [] p in
      ObjLike body
  in
  H.replace st.macro_tab name def

let unget_lexeme (lexbuf : Lexing.lexbuf) =
  lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
  lexbuf.lex_curr_p <- lexbuf.lex_start_p

let handle_include st p lexbuf =
  let parse lexbuf =
    let search_path =
      match Lexer.include_file lexbuf with
      | SysInclude -> st.sys_include_dirs
      | UserInclude -> st.user_include_dirs
    in
    let s = Lexing.lexeme lexbuf in
    let filename = String.sub s 1 (String.length s - 2) in
    begin match find_include_file search_path filename with
      | Some path ->
        if List.length st.include_stack >= st.max_include_depth then
          error lexbuf.lex_start_p "too many nested #include's";
        push_include st path
      | None ->
        error lexbuf.lex_start_p ("cannot find " ^ s)
    end
  in
  try parse lexbuf
  with Lexer.Error -> (* need macro expansion *)
    (* put back the byte taken out by Lexer.include_file *)
    (* this must be done before reading any token from p *)
    unget_lexeme lexbuf;
    skip p; (* "include" *)
    let pos = p.tok.pos in
    (* fully macro-expand the rest of the directive *)
    let buf = Buffer.create 16 in
    let rec loop () =
      match getsym_expand st.macro_tab p with
      | { kind = EOF; _ } -> ()
      | { text; ws; _ } ->
        Buffer.add_string buf ws;
        Buffer.add_string buf text;
        loop ()
    in
    loop ();
    let s = flush_buffer buf in
(*     Printf.eprintf "‘%s’\n" s; *)
    let lexbuf = lexbuf_of_string pos s in
    try parse lexbuf
    with Lexer.Error ->
      error pos "syntax error"

let handle_directive st (pos : Lexing.position) dir =
(*   Printf.eprintf "directive: %s" dir; *)
  let ws_buf = Buffer.create 16 in
  let lexbuf = lexbuf_of_string pos dir in
  let p = make_parser (fun () -> lex ws_buf lexbuf) in
  match st.cond_stack with
  | [] | (true, _) :: _ ->
    begin match p.tok.text with
      | "define" -> handle_define st p
      | "undef" ->
        skip p;
        let name = expect_ident p in
        expect_eof p;
        H.remove st.macro_tab name
      | "ifdef" ->
        skip p;
        let name = expect_ident p in
        expect_eof p;
        st.cond_stack <-
          let active = H.mem st.macro_tab name in
          (active, if active then After else Before) :: st.cond_stack
      | "ifndef" ->
        skip p;
        let name = expect_ident p in
        expect_eof p;
        st.cond_stack <-
          let active = not (H.mem st.macro_tab name) in
          (active, if active then After else Before) :: st.cond_stack
      | "elif" -> handle_elif st p
      | "else" -> handle_else st p
      | "endif" -> handle_endif st p
      | "include" -> handle_include st p lexbuf
      | "if" ->
        skip p;
        let active =
          try
            parse_cond (make_cond_expander st.macro_tab p) <> 0L
          with Error (pos, msg) ->
            (* when the condition is malformed, consider it false *)
            report_error pos msg;
            false
        in
        st.cond_stack <- (active, if active then After else Before) :: st.cond_stack
      | "warning" -> () (* TODO *)
      | "error" -> () (* TODO *)
      | "" -> () (* null directive *)
      | _ -> error p.tok.pos "invalid directive"
    end
  | (false, _) :: _ ->
    begin match p.tok.text with
      | "if" | "ifdef" | "ifndef" ->
        st.cond_stack <- (false, After) :: st.cond_stack
      | "elif" -> handle_elif st p
      | "else" -> handle_else st p
      | "endif" -> handle_endif st p
      | _ -> ()
      (*       | d -> Format.eprintf "skipping directive %s@." d *)
    end

let make_preproc_parser st =
  let dir_buf = Buffer.create 256 in
  let rec next () =
    (* token queue is empty now *)
    let t =
      match st.cond_stack with
      | [] | (true, _) :: _ ->
        lex st.ws_buf st.lexbuf
      | (false, _) :: _ ->
(*      let lb = st.lexbuf in
        Format.eprintf "about to skip, current position: %a@." pp_pos lb.lex_curr_p;
        let buf = Bytes.sub_string lb.lex_buffer lb.lex_curr_pos lb.lex_buffer_len in
        Format.eprintf "lexbuf contents: ‘%s’@." buf; *)
        if Lexer.skip_to_directive true st.lexbuf then
          wrap_token_no_ws Directive st.lexbuf
        else
          error st.lexbuf.lex_curr_p "unterminated #if"
    in
    match t.kind with
    | Directive ->
      let dir_pos = st.lexbuf.lex_curr_p in
      Lexer.directive dir_buf st.lexbuf;
(*       Format.eprintf "current position: %a@." pp_pos st.lexbuf.lex_curr_p; *)
      let dir = flush_buffer dir_buf in
      assert (st.lexbuf.lex_curr_p.pos_cnum = st.lexbuf.lex_curr_p.pos_bol);
      begin
        try
          handle_directive st dir_pos dir
        with Error (pos, msg) ->
          report_error pos msg
      end;
      next ()
    | EOF ->
      if st.include_stack = [] then
        { t with ws = flush_buffer st.ws_buf }
      else begin
        if st.cond_stack <> [] then error t.pos "unterminated #if";
        pop_include st;
        next ()
      end
    | _ -> t
  in
  make_parser next

let make_supplier ic filename =
  let st = init_state ic filename in
  let p = make_preproc_parser st in
  fun () ->
    let token = getsym_expand st.macro_tab p in
(*     Format.printf "%d: %a ‘%s’\n" token.pos pp_pptoken token.kind token.text; *)
    let start_pos = token.pos in
    let end_pos =
      { start_pos with
        pos_cnum = start_pos.pos_cnum + String.length token.text }
    in
    let token' = convert_token token.kind token.text in
    (* let line = start_pos.pos_lnum in
    let col = start_pos.pos_cnum - start_pos.pos_bol in *)
    (* Format.printf "%d:%d: %a ‘%s’\n"
      line col pp_pptoken token.kind token.text; *)
    token', start_pos, end_pos
