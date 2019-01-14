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
  ws : string (* preceding whitespace *)
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
  | Inactive

type state = {
  ws_buf : Buffer.t;
  mutable lexbuf : Lexing.lexbuf;
  macro_tab : macro_def H.t;
  (* fst indicates whether in active branch *)
  mutable cond_stack : (bool * cond_state) list;
  sys_include_dirs : string list;
  user_include_dirs : string list;
  mutable input_chan : in_channel;
  mutable include_stack : (Lexing.lexbuf * in_channel) list;
  max_include_depth : int
}

let init_state input_chan filename =
  let lexbuf = Lexing.from_channel input_chan in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with
      pos_fname = filename };
  let macro_tab =
    [
      "__FILE__", ObjLike [Magic_FILE];
      "__LINE__", ObjLike [Magic_LINE]
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

let at_bol p =
  Lexing.(p.pos_cnum = p.pos_bol)

let flush_buffer b =
  let s = Buffer.contents b in
  Buffer.clear b;
  s

let lex ws_buf lexbuf =
  let kind = Lexer.token ws_buf lexbuf in
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_p in
  let ws =
    if kind = Hash && at_bol pos || kind = EOF then "" else
      flush_buffer ws_buf
  in
(*   Format.eprintf "%a: ‘%s’ ‘%s’@." pp_pos pos ws text; *)
(*   Format.eprintf "%a: ‘%s’@." pp_pos pos text; *)
  { kind; text; pos; ws }

exception Error of Lexing.position * string

let error pos s = raise (Error (pos, s))

let pp_token_list f l =
  let open Format in
  l |> List.iter begin fun t ->
    pp_print_string f t.ws;
    pp_print_string f t.text
  end

(* token stream *)
type parser_ = {
  next : unit -> token;
  mutable tok : token;
  mutable tokq : token list
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
  | Ident _ -> t.text
  | _ -> error t.pos "identifier expected"

let parse_macro_arg p =
  let rec loop depth acc =
    let t = p.tok in
    match t.kind with
    | Comma when depth = 0 -> acc
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

let parse_macro_arg_list p =
  skip p; (* '(' *)
  match p.tok.kind with
  | RParen -> skip p; []
  | _ ->
    let rec loop () =
      let arg = parse_macro_arg p in
      match getsym p with
      | { kind = RParen; _ } -> [arg]
      | { kind = Comma; _ } -> arg :: loop ()
      | { pos; _ } -> error pos "macro argument list"
    in
    loop ()

(* s is guaranteed to be non-empty *)
let parse_token s =
  let lexbuf = Lexing.from_string s in
  let ws_buf = Buffer.create 0 in
  let t = Lexer.token ws_buf lexbuf in
  if lexbuf.lex_start_p.pos_cnum > 0 ||
     lexbuf.lex_curr_p.pos_cnum < String.length s
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
    pos; ws = "" }

let get_noexp_list = function
  | { kind = Ident l; _ } -> l
  | _ -> []

let concat_token pos t1 t2 =
  let text = t1.text ^ t2.text in
  let kind =
    match parse_token text with
    | Ident _ -> Ident (get_noexp_list t1 @ get_noexp_list t2)
    | k -> k
  in
  { kind; text; pos; ws = "" }

let rec subst_token (macro_name, noexp, pos, ws as token_info) arg_tab = function
  | Verbatim ({ kind = Ident _; _ } as t) ->
    [{ t with kind = Ident (macro_name :: noexp); pos }]
  | Verbatim t ->
    [{ t with pos }]
  | Param i ->
    arg_tab.(i) |> List.map
      (function
        | { kind = Ident l; _ } as t ->
          { t with kind = Ident (macro_name :: l) }
        | t -> t)
  | Stringify (ws, i) ->
    [stringify pos ws arg_tab.(i)]
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
    [{ kind = StringLit; text = quote pos.pos_fname; pos; ws }]
  | Magic_LINE ->
    [{ kind = IntLit; text = string_of_int pos.pos_lnum; pos; ws }]

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
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with
      pos_fname = path };
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

let is_oct_digit = function
  | '0'..'7' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let digit_value = function
  | '0'..'9' as c -> Char.code c - 48
  | _ -> assert false

let parse_int s =
  let n = String.length s in
  let i = ref 0 in
  let tmp = ref 0 in
  if s.[0] = '0' then begin
    (* octal *)
    i := 1;
    while !i < n && is_oct_digit s.[!i] do
      tmp := !tmp*8 + digit_value s.[!i];
      incr i
    done
  end else begin
    while !i < n && is_digit s.[!i] do
      tmp := !tmp*10 + digit_value s.[!i];
      incr i
    done
  end;
  !tmp

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
    while !j < n && is_oct_digit s.[!j] do
      tmp := !tmp*8 + digit_value s.[!j];
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

let is_prefix_op = function
  | Minus | Tilde | Bang -> true
  | _ -> false

let eval_prefix_op op v =
  match op with
  | Minus -> -v
  | Tilde -> lnot v
  | Bang -> if v=0 then 1 else 0
  | _ -> assert false

let eval_binary_op op v1 v2 =
  match op.kind with
  | Star -> v1*v2
  | Slash ->
    if v2 = 0 then error op.pos "division by 0" else v1/v2
  | Percent ->
    if v2 = 0 then error op.pos "division by 0" else v1 mod v2
  | Plus -> v1+v2
  | Minus -> v1-v2
  | LtLt -> v1 lsl v2
  | GtGt -> v2 lsr v2
  | Lt -> if v1<v2 then 1 else 0
  | Gt -> if v1>v2 then 1 else 0
  | LtEq -> if v1 <= v2 then 1 else 0
  | GtEq -> if v1 >= v2 then 1 else 0
  | EqEq -> if v1 = v2 then 1 else 0
  | BangEq -> if v1 <> v2 then 1 else 0
  | And -> v1 land v2
  | Circ -> v1 lxor v2
  | Pipe -> v1 lor v2
  | AndAnd -> if v1<>0 && v2<>0 then 1 else 0
  | PipePipe -> if v1<>0 || v2<>0 then 1 else 0
  | _ -> assert false

let parse_binary_expr parse_sub_expr op_test p =
  let v1 = parse_sub_expr p in
  let rec loop v1 =
    if op_test p.tok.kind then begin
      let op = p.tok in
      skip p;
      let v2 = parse_sub_expr p in
      loop (eval_binary_op op v1 v2)
    end else v1
  in
  loop v1

let rec parse_atom p =
  match getsym p with
  | { kind = IntLit; text; _ } ->
    parse_int text
  | { kind = CharLit; text; _ } ->
    parse_char text
  | { kind = LParen; _ } ->
    let v = parse_cond_expr p in
    expect p ")";
    v
  | { pos; _ } -> error pos "invalid token"

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
    (function Plus | Minus -> true | _ -> false) p
and parse_shift_expr p = parse_binary_expr parse_add_expr
    (function LtLt | GtGt -> true | _ -> false) p
and parse_rel_expr p = parse_binary_expr parse_shift_expr
    (function Lt | Gt | LtEq | GtEq -> true | _ -> false) p
and parse_eq_expr p = parse_binary_expr parse_rel_expr
    (function EqEq | BangEq -> true | _ -> false) p
and parse_and_expr p = parse_binary_expr parse_eq_expr ((=) And) p
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
    if v1 <> 0 then v2 else v3
  end else v1

let parse_cond p =
  let v = parse_cond_expr p in
  expect p "";
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

let rec parse_macro_body param_map p =
  let parse_simple () =
  match getsym p with
  | { kind = Ident _; text = name; _ } as t ->
    begin match M.find name param_map with
      | i -> Param i
      | exception Not_found -> Verbatim t
    end
  | { kind = Hash; ws; _ } as t ->
    let name = expect_ident p in
    begin match M.find name param_map with
      | i -> Stringify (ws, i)
      | exception Not_found -> Verbatim t
    end
  | t -> assert (t.kind <> EOF); Verbatim t
  in
  let rec parse_binary () =
    let rt1 = parse_simple () in
    if p.tok.kind = HashHash then
      let () = skip p in
      let rt2 = parse_simple () in
      Concat (rt1, rt2)
    else rt1
  in
  let rec loop acc =
    if p.tok.kind = EOF then acc else
      loop (parse_binary () :: acc)
  in
  loop [] |> List.rev

let rec getsym_expand macro_tab p =
  let t = getsym p in
  match t.kind with
  | Ident noexp when subject_to_expansion macro_tab p t.text noexp ->
    expand_ident macro_tab p t;
    getsym_expand macro_tab p
  | _ -> t

and expand_ident macro_tab p t : unit =
  let noexp = match t.kind with Ident l -> l | _ -> assert false in
  let s = t.text in
  let expand arg_tab body =
    let l' =
      body |> List.map (subst_token (s, noexp, t.pos, t.ws) arg_tab) |> List.concat
    in
    begin match l' with
      | [] -> []
      | hd::tl -> { hd with ws = t.ws } :: tl
    end
  in
  let l =
    match H.find macro_tab s with
    | ObjLike body -> expand [||] body
    | FunLike (arity, body) ->
      assert (p.tok.kind = LParen);
      let arg_tab =
        let args = parse_macro_arg_list p in
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
  in
  unget_token_list p l

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
        | { kind = Ident _; text; _ } -> text
        | { pos; _ } -> error pos "syntax error"
      in
      let text = if H.mem macro_tab name then "1" else "0" in
      { t with kind = IntLit; text }
    | { kind = Ident _ } as t ->
      { t with kind = IntLit; text = "0" }
    | t -> t
  end

let handle_else st p =
  let pos = p.tok.pos in
  skip p;
  expect p "";
  match st.cond_stack with
  | [] -> error pos "#else without #if"
  | (_, Else) :: _ -> error pos "duplicate #else"
  | (_, Before) :: tl -> st.cond_stack <- (true, Else) :: tl
  | (_, After) :: tl -> st.cond_stack <- (false, Else) :: tl
  | (_, Inactive) :: _ -> ()

let handle_endif st p =
  let pos = p.tok.pos in
  skip p;
  expect p "";
  match st.cond_stack with
  | [] -> error pos "#endif without #if"
  | _::tl -> st.cond_stack <- tl

let handle_elif st p =
  let pos = p.tok.pos in
  skip p;
  match st.cond_stack with
  | [] -> error pos "#elif without #if"
  | (_, Else) :: _ -> error pos "#elif after #else"
  | (true, After) :: tl -> st.cond_stack <- (false, After) :: tl
  | (false, After) :: _ -> ()
  | (_, Before) :: tl ->
    let active = parse_cond (make_cond_expander st.macro_tab p) <> 0 in
    st.cond_stack <- (active, if active then After else Before) :: tl
  | (_, Inactive) :: _ -> ()

let handle_directive st (pos : Lexing.position) dir =
(*   Printf.eprintf "directive: %s" dir; *)
  let ws_buf = Buffer.create 16 in
  let lexbuf = Lexing.from_string dir in
  lexbuf.lex_abs_pos <- pos.pos_cnum;
  lexbuf.lex_curr_p <- pos;
  let p = make_parser (fun () -> lex ws_buf lexbuf) in
  match st.cond_stack with
  | [] | (true, _) :: _ ->
    begin match p.tok.text with
      | "define" ->
        skip p;
        let name = expect_ident p in
        let def =
          match p.tok with
          | { kind = LParen; ws = ""; _ } ->
            skip p;
            (* function-like macro *)
            let param_map, arity =
              match getsym p with
              | { kind = RParen; _ } -> M.empty, 0
              | { kind = Ident _; text = s; _ } ->
                let rec loop m i =
                  match getsym p with
                  | { kind = Comma; _ } ->
                    let s = expect_ident p in
                    loop (M.add s i m) (i+1)
                  | { kind = RParen; _ } -> m, i
                  | { pos; _ } -> error pos "invalid token"
                in
                loop (M.singleton s 0) 1
              | { pos; _ } -> error pos "invalid token"
            in
            let body = parse_macro_body param_map p in
            FunLike (arity, body)
          | t ->
            (* object-like macro *)
            let body = parse_macro_body M.empty p in
            ObjLike body
        in
        H.replace st.macro_tab name def
      | "undef" ->
        skip p;
        let name = expect_ident p in
        expect p "";
        H.remove st.macro_tab name
      | "ifdef" ->
        skip p;
        let name = expect_ident p in
        expect p "";
        st.cond_stack <-
          let active = H.mem st.macro_tab name in
          (active, if active then After else Before) :: st.cond_stack
      | "ifndef" ->
        skip p;
        let name = expect_ident p in
        expect p "";
        st.cond_stack <-
          let active = not (H.mem st.macro_tab name) in
          (active, if active then After else Before) :: st.cond_stack
      | "elif" -> handle_elif st p
      | "else" -> handle_else st p
      | "endif" -> handle_endif st p
      | "include" ->
        let get_filename lexbuf =
          let s = Lexing.lexeme lexbuf in
          String.sub s 1 (String.length s - 2)
        in
        begin
          try
            match Lexer.include_file lexbuf with
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
          with Lexer.Error ->
            error lexbuf.lex_start_p "syntax error"
        end
      | "if" ->
        skip p;
        let active = parse_cond (make_cond_expander st.macro_tab p) <> 0 in
        st.cond_stack <- (active, if active then After else Before) :: st.cond_stack
      | "warning" -> () (* TODO *)
      | "error" -> () (* TODO *)
      | "" -> () (* null directive *)
      | _ -> error p.tok.pos "invalid directive"
    end
  | (false, _) :: _ ->
    begin match p.tok.text with
      | "if" | "ifdef" | "ifndef" ->
        st.cond_stack <- (false, Inactive) :: st.cond_stack
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
        if Lexer.skip_to_directive st.lexbuf then
          wrap_token_no_ws Hash st.lexbuf
        else
          error st.lexbuf.lex_curr_p "unterminated #if"
    in
    match t.kind with
    | Hash when at_bol t.pos ->
      let dir_pos = st.lexbuf.lex_curr_p in
      Lexer.directive dir_buf st.lexbuf;
(*       Format.eprintf "current position: %a@." pp_pos st.lexbuf.lex_curr_p; *)
      let dir = flush_buffer dir_buf in
      assert (st.lexbuf.lex_curr_p.pos_cnum = st.lexbuf.lex_curr_p.pos_bol);
      begin
        try
          handle_directive st dir_pos dir
        with Error (pos, msg) ->
          Format.eprintf "%a: %s@." pp_pos pos msg;
      end;
      next ()
    | EOF ->
      if st.include_stack = [] then
        { t with ws = flush_buffer st.ws_buf }
      else begin
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
    let line = start_pos.pos_lnum in
    let col = start_pos.pos_cnum - start_pos.pos_bol in
    Format.printf "%d:%d: %a ‘%s’\n"
      line col pp_pptoken token.kind token.text;
    token', start_pos, end_pos

let main () =
  let st = init_state stdin "<stdin>" in
  let p = make_preproc_parser st in
  let rec loop () =
    let t = getsym_expand st.macro_tab p in
    print_string t.ws;
    print_string t.text;
    if t.kind = EOF then () else loop ()
  in
  try loop ()
  with Failure msg ->
    Format.eprintf "Failure ‘%s’ raised at %a@."
      msg pp_pos st.lexbuf.lex_curr_p;
    exit 1
