open Token
open Util

module H = String_Table

type token' = {
  kind : token;
  text : string;
  pos : Lexing.position;
  ws : string (* preceding whitespace *)
}

let token_end_pos tok =
  { tok.pos with
    pos_cnum = tok.pos.pos_cnum + String.length tok.text }

type replace_token =
  | Verbatim of token'
  | Param of string(*whitespace*) * int
  | Stringify of string(*whitespace*) * int
  | Concat of replace_token * replace_token
  | Magic_FILE
  | Magic_LINE

let rec pp_replace_token f =
  let open Format in
  function
  | Verbatim tok ->
    if tok.ws <> "" then pp_print_char f ' ';
    pp_print_string f tok.text
  | Param (ws, i) ->
    if ws <> "" then pp_print_char f ' ';
    fprintf f "$%d" (1+i)
  | Stringify (ws, i) ->
    if ws <> "" then pp_print_char f ' ';
    fprintf f "$STRING($%d)" (1+i)
  | Concat (rtok1, rtok2) ->
    fprintf f "$CONCAT(%a,%a)" pp_replace_token rtok1 pp_replace_token rtok2
  | Magic_FILE -> pp_print_string f "$FILE"
  | Magic_LINE -> pp_print_string f "$LINE"

type macro_body =
  | ObjLike of replace_token list
  | FunLike of int * bool * replace_token list

let pp_macro_body f = function
  | ObjLike toklist
  | FunLike (_, _, toklist) ->
    List.iter (pp_replace_token f) toklist

type macro_def_loc =
  | Loc_Builtin
  | Loc_Source of string(*filename*) * int(*line*)

type macro_def = macro_body * macro_def_loc

let macro_def_body : macro_def -> macro_body = fst
let macro_def_loc : macro_def -> macro_def_loc = snd

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

type config = {
  sys_include_dirs : string list;
  user_include_dirs : string list
}

let init_state conf input_chan filename =
  let lexbuf = Lexing.from_channel input_chan in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with
      pos_fname = filename };
  let macro_tab =
    let r kind text =
      [Verbatim { kind; text; ws = ""; pos = Lexing.dummy_pos }]
    in [
      "__FILE__", (ObjLike [Magic_FILE], Loc_Builtin);
      "__LINE__", (ObjLike [Magic_LINE], Loc_Builtin);
      "__STDC__", (ObjLike (r (INT_LIT "1") "1"), Loc_Builtin);
      "__STDC_VERSION__", (ObjLike (r (INT_LIT "199901L") "199901L"), Loc_Builtin);
    ] |> List.to_seq |> H.of_seq
  in
  { ws_buf = Buffer.create 1024;
    lexbuf;
    include_stack = [];
    macro_tab;
    cond_stack = [];
    sys_include_dirs = conf.sys_include_dirs;
    user_include_dirs = conf.user_include_dirs;
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

let raw_lex ws_buf lexbuf =
  let kind = Lexer.token ws_buf lexbuf in
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_p in
  let ws = Buffer.contents ws_buf in
  { kind; text; pos; ws }

let lex ws_buf lexbuf =
  let { kind; text; pos; ws = ws0 } = raw_lex ws_buf lexbuf in
  let tok =
    if kind = HASH && at_bol pos ws0 then
      { kind = DIRECTIVE; text; pos; ws = "" }
    else if kind = EOF then
      { kind; text; pos; ws = "" }
    else
      let () = Buffer.clear ws_buf in
      { kind; text; pos; ws = ws0 }
  in
  if !Global.debug_preproc then
    Format.eprintf "[lex] %a: %a ‘%s%s’@." pp_pos tok.pos
      Token.pp_token tok.kind tok.ws tok.text;
  tok

exception Error of Lexing.position * string

let error pos s = raise (Error (pos, s))

let report_error pos msg =
  Format.eprintf "%a: %s@." pp_pos pos msg

let unexpected_token pos = error pos "unexpected token"

let pp_token f tok =
  Format.pp_print_string f tok.ws;
  Format.pp_print_string f tok.text

let pp_token_array f tok_arr =
  Array.iter (pp_token f) tok_arr

type token_or_whitespace = Token of token' | Whitespace of string

let pp_token_or_whitespace f = function
  | Token tok -> pp_token f tok
  | Whitespace ws -> Format.pp_print_string f ws

let pp_token_or_whitespace_array f arr =
  Array.iter (pp_token_or_whitespace f) arr

(* token stream *)
type parser = {
  next : unit -> token';
  mutable tok : token';
  mutable tokq : token_or_whitespace list
}

let rec skip p =
  match p.tokq with
  | [] -> p.tok <- p.next ()
  | Token hd :: tl ->
    p.tokq <- tl;
    p.tok <- hd
  | Whitespace ws :: tl ->
    p.tokq <- tl;
    skip p;
    let tok = p.tok in
    p.tok <- { tok with ws = ws ^ tok.ws }

let getsym p =
  let t = p.tok in
  skip p; t

let unget_token p tok =
  p.tokq <- Token p.tok :: p.tokq;
  p.tok <- tok

let unget_token_or_whitespace p tok_or_ws =
  match tok_or_ws with
  | Token tok -> unget_token p tok
  | Whitespace ws ->
    let tok = p.tok in
    p.tok <- { tok with ws = ws ^ tok.ws }

let expect p text =
  if p.tok.text = text then skip p else
    error p.tok.pos (Printf.sprintf "‘%s’ expected" text)

let expect_ident p =
  let t = getsym p in
  match t.kind with
  | PREIDENT _ -> t.text
  | _ -> error t.pos "identifier expected"

let expect_eof p =
  if p.tok.kind <> EOF then
    error p.tok.pos "extra token"

let parse_macro_arg ~stop_at_comma p =
  let rec loop depth acc =
    let t = p.tok in
    match t.kind with
    | COMMA when stop_at_comma && depth = 0 -> acc
    | LPAREN -> skip p; loop (depth+1) (t :: acc)
    | RPAREN ->
      if depth = 0 then acc else begin
        skip p;
        loop (depth-1) (t :: acc)
      end
    | EOF -> error t.pos "unterminated macro argument"
    | _ -> skip p; loop depth (t :: acc)
  in
  let l = loop 0 [] in
  List.rev l |> Array.of_list

(* return value in reverse order *)
let parse_macro_arg_list arity p =
  skip p; (* '(' *)
  match p.tok.kind with
  | RPAREN -> skip p; []
  | _ ->
    let rec loop n acc =
      let arg = parse_macro_arg ~stop_at_comma:(n < arity) p in
      match getsym p with
      | { kind = RPAREN; _ } -> arg :: acc
      | { kind = COMMA; _ } -> loop (n+1) (arg :: acc)
      | { pos; _ } -> error pos "unexpected_token in macro argument list"
    in
    loop 0 []

let lexbuf_of_string (pos : Lexing.position) s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_abs_pos <- pos.pos_cnum;
  lexbuf.lex_curr_p <- pos;
  lexbuf

(* s is guaranteed to be non-empty *)
let parse_token s =
  let lexbuf = Lexing.from_string s in
  let ws_buf = Buffer.create 0 in
  let tok = raw_lex ws_buf lexbuf in
  (tok, lexbuf.lex_curr_p.pos_cnum < String.length s)

let parse_token_strict s =
  let (tok, has_trail) = parse_token s in
  if tok.ws <> "" || has_trail then None
  else Some tok

let quote s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  s |> String.iter begin fun c ->
    if c = '"' || c = '\\' then Buffer.add_char buf '\\';
    Buffer.add_char buf c
  end;
  Buffer.add_char buf '"';
  Buffer.contents buf

let stringify pos ws tok_arr =
  let len = Array.length tok_arr in
  let s =
    if len = 0 then ""
    else begin
      let buf = Buffer.create 16 in
      (* whitespace before first token is deleted *)
      Buffer.add_string buf tok_arr.(0).text;
      for i=1 to len-1 do
        let t = tok_arr.(i) in
        if t.ws <> "" then Buffer.add_char buf ' ';
        Buffer.add_string buf t.text
      done;
      Buffer.contents buf
    end
  in
  { kind = STRING_LIT s; text = quote s; pos; ws }

let noexp_list = function
  | { kind = PREIDENT l; _ } -> l
  | _ -> []

exception Invalid_Token

let concat_token pos t1 t2 =
  let text = t1.text ^ t2.text in
  match parse_token_strict text with
  | Some tok ->
    begin match tok.kind with
      | PREIDENT _ ->
        { tok with pos; kind = PREIDENT (noexp_list t1 @ noexp_list t2) }
      | _ -> { tok with pos }
    end
  | None ->
    raise Invalid_Token

let mark_token macro_name = function
  | { kind = PREIDENT l; _ } as t ->
    { t with kind = PREIDENT (macro_name :: l) }
  | t -> t

let rec subst_token (macro_name, noexp, pos as token_info)
    arg_tab ~verbatim = function
  (* token_info is about the identifier token being expanded *)
  | Verbatim ({ kind = PREIDENT noexp; _ } as t) ->
    [| Token { t with kind = PREIDENT (macro_name :: noexp); pos } |]
  | Verbatim t ->
    [| Token { t with pos } |]
  | Param (ws, i) ->
    let arg = (if verbatim then fst else snd) arg_tab.(i) in
    (* replace whitespace and add no-expand mark to tokens *)
    let len = Array.length arg in
    if len = 0 then [| Whitespace ws |]
    else begin
      let first_tok = arg.(0) in
      arg.(0) <- { first_tok with ws };
      let result = Array.map (fun t -> Token (mark_token macro_name t)) arg in
      arg.(0) <- first_tok; result
    end
  | Stringify (ws, i) ->
    [| Token (stringify pos ws (fst arg_tab.(i))) |]
  | Concat (u1, u2) ->
    let u1' = subst_token token_info arg_tab ~verbatim:true u1 in
    let u2' = subst_token token_info arg_tab ~verbatim:true u2 in
    let len_left  = Array.length u1'
    and len_right = Array.length u2' in
    let rec find_left i =
      if i >= 0 then
        match u1'.(i) with
        | Token _ -> Some i
        | Whitespace _ -> find_left (i-1)
      else None
    in
    let rec find_right i =
      if i < len_right then
        match u2'.(i) with
        | Token _ -> Some i
        | Whitespace _ -> find_right (i+1)
      else None
    in
    begin match find_left (len_left-1), find_right 0 with
      | None, None -> [||]
      | Some i, None -> Array.sub u1' 0 (i+1)
      | None, Some i ->
        let arr = Array.sub u2' i (len_right-i) in
        let [@warning "-8"] Token tok = arr.(0) in
        arr.(0) <- Token { tok with ws="" }; arr
      | Some i, Some j ->
        let [@warning "-8"] Token tok1, Token tok2 = (u1'.(i), u2'.(j)) in
        begin
          try
            let new_tok = concat_token pos tok1 tok2 in
            let arr = Array.make (i + (len_right - j)) (Whitespace "") in
            Array.blit u1' 0 arr 0 i;
            Array.blit u2' j arr i (len_right - j);
            arr.(i) <- Token new_tok; arr
          with Invalid_Token ->
            report_error tok1.pos
              (Printf.sprintf "concatenating ‘%s’ and ‘%s’ does not yield a valid token"
                 tok1.text tok2.text);
            Array.concat [u1';u2']
        end
    end
  | Magic_FILE ->
    let s = pos.pos_fname in
    [| Token { kind = STRING_LIT s; text = quote s; pos; ws = "" } |]
  | Magic_LINE ->
    let s = string_of_int pos.pos_lnum in
    [| Token { kind = INT_LIT s; text = s; pos; ws = "" } |]

let rec find_include_file search_path filename =
  match search_path with
  | [] -> Printf.eprintf "%s not found\n" filename; None
  | hd :: tl ->
    let path = hd ^ filename in
(*  Printf.eprintf "trying %s\n" path;*)
    if Sys.file_exists path then Some path else
      find_include_file tl filename

let push_include st path =
  let inchan = open_in path in
  st.include_stack <-
    (st.lexbuf, st.input_chan, st.cond_stack) :: st.include_stack;
  let lexbuf = Lexing.from_channel inchan in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with
      pos_fname = path };
  st.lexbuf <- lexbuf;
  st.input_chan <- inchan;
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
  | MINUS | TILDE | BANG -> true
  | _ -> false

let eval_prefix_op op v =
  match op with
  | MINUS -> Int64.neg v
  | TILDE -> Int64.lognot v
  | BANG -> if v=0L then 1L else 0L
  | _ -> assert false

let eval_binary_op op v1 v2 =
  match op.kind with
  | STAR -> Int64.mul v1 v2
  | SLASH ->
    if v2 = 0L then error op.pos "division by 0" else Int64.div v1 v2
  | PERCENT ->
    if v2 = 0L then error op.pos "division by 0" else Int64.rem v1 v2
  | PLUS -> Int64.add v1 v2
  | MINUS -> Int64.sub v1 v2
  | LTLT -> Int64.shift_left v1 (Int64.to_int v2)
  | GTGT -> Int64.shift_right v1 (Int64.to_int v2)
  | LT -> if v1<v2 then 1L else 0L
  | GT -> if v1>v2 then 1L else 0L
  | LTEQ -> if v1 <= v2 then 1L else 0L
  | GTEQ -> if v1 >= v2 then 1L else 0L
  | EQEQ -> if v1 = v2 then 1L else 0L
  | BANGEQ -> if v1 <> v2 then 1L else 0L
  | AMP -> Int64.logand v1 v2
  | CIRC -> Int64.logxor v1 v2
  | BAR -> Int64.logor v1 v2
  | AMPAMP -> if v1<>0L && v2<>0L then 1L else 0L
  | BARBAR -> if v1<>0L || v2<>0L then 1L else 0L
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
  | { kind = INT_LIT _; text; _ } ->
    parse_int text
  | { kind = CHAR_LIT _; text; _ } ->
    Int64.of_int (parse_char text)
  | { kind = LPAREN; _ } ->
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
    (function STAR | SLASH | PERCENT -> true | _ -> false) p
and parse_add_expr p = parse_binary_expr parse_mult_expr
    (function PLUS | MINUS -> true | _ -> false) p
and parse_shift_expr p = parse_binary_expr parse_add_expr
    (function LTLT | GTGT -> true | _ -> false) p
and parse_rel_expr p = parse_binary_expr parse_shift_expr
    (function LT | GT | LTEQ | GTEQ -> true | _ -> false) p
and parse_eq_expr p = parse_binary_expr parse_rel_expr
    (function EQEQ | BANGEQ -> true | _ -> false) p
and parse_and_expr p = parse_binary_expr parse_eq_expr ((=) AMP) p
and parse_xor_expr p = parse_binary_expr parse_and_expr ((=) CIRC) p
and parse_or_expr p = parse_binary_expr parse_xor_expr ((=) BAR) p
and parse_log_and_expr p = parse_binary_expr parse_or_expr ((=) AMPAMP) p
and parse_log_or_expr p = parse_binary_expr parse_log_and_expr ((=) BARBAR) p

and parse_cond_expr p =
  let v1 = parse_log_or_expr p in
  if p.tok.kind = QUEST then begin
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

let subject_to_expansion macro_tab p text noexp =
  not (List.mem text noexp) &&
  match macro_def_body (H.find macro_tab text) with
  | ObjLike _ -> true
  | FunLike _ -> p.tok.kind = LPAREN
  | exception Not_found -> false

let make_parser next =
  { next; tok = next (); tokq = [] }

let parse_macro_body param_alist p =
  let parse_simple () =
    match getsym p with
    | { kind = PREIDENT _; text = name; ws; _ } as t ->
      begin match List.assoc name param_alist with
        | i -> Param (ws, i)
        | exception Not_found -> Verbatim t
      end
    | { kind = HASH; ws; _ } as hash ->
      (* According to the standard, each '#' in a function-like macro must be
         followed by a parameter. Here we don't enforce this constraint. *)
      begin match p.tok.kind with
        | PREIDENT _ ->
          let name = p.tok.text in
          begin match List.assoc name param_alist with
            | i -> skip p; Stringify (ws, i)
            | exception Not_found -> Verbatim hash
          end
        | _ -> Verbatim p.tok
      end
    | t ->
      assert (t.kind <> EOF);
      Verbatim t
  in
  let parse_binary () =
    let rec loop rt1 =
      if p.tok.kind = HASHHASH then
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

let rev_iter_array f arr =
  let n = Array.length arr in
  for i=n-1 downto 0 do f arr.(i) done

let without_whitespace tok = { tok with ws="" }

let rec getsym_expand macro_tab p =
  let t = getsym p in
  match t.kind with
  | PREIDENT noexp when subject_to_expansion macro_tab p t.text noexp ->
    expand_ident macro_tab p t;
    getsym_expand macro_tab p
  | _ -> t

and expand_ident macro_tab p tok : unit =
  let noexp = match tok.kind with PREIDENT l -> l | _ -> assert false in
  let macro_name = tok.text in
  let expand arg_tab macro_body =
    macro_body |> List.map (subst_token (macro_name, noexp, tok.pos) arg_tab ~verbatim:false)
    |> Array.concat
  in
  let arg_tab, macro_body =
    match macro_def_body (H.find macro_tab macro_name) with
    | ObjLike macro_body -> [||], macro_body
    | FunLike (arity, is_vararg, macro_body) ->
      assert (p.tok.kind = LPAREN);
      (* length of arg_tab should be arity+1 *)
      let args = parse_macro_arg_list arity p in
      let n_arg = List.length args in
      let args' =
        let rec fill_missing_args args n =
          if n <= 0 then args else fill_missing_args ([||]::args) (n-1)
        in
        assert (n_arg <= arity+1);
        if is_vararg then begin
          if n_arg = arity + 1 then args else
            fill_missing_args args (arity + 1 - n_arg)
            (* error tok.pos "wrong number of macro arguments" *)
        end else begin
          if n_arg = arity then args else
            fill_missing_args args (arity - n_arg)
            (* error tok.pos "wrong number of macro arguments" *)
        end
      in
      let arg_tab =
        args' |> List.map (fun l -> l, expand_token_array macro_tab l) |>
        List.rev |> Array.of_list
      in
      (*for i=0 to n_arg-1 do
        let arg_verbatim, arg_expanded = arg_tab.(i) in
        let remove_leading_ws arr =
          if Array.length arr > 0 then arr.(0) = without_whitespace arr.(0)
        in
        remove_leading_ws arg_verbatim;
        remove_leading_ws arg_expanded
      done*)
      if !Global.debug_preproc then begin
        Format.eprintf "about to expand ‘%s’ at %a@." macro_name
          pp_pos tok.pos;
        let n_arg = Array.length arg_tab in
        for i=0 to n_arg-1 do
          let verbatim, expanded = arg_tab.(i) in
          Format.eprintf "  $%d: verbatim ‘%a’, expanded ‘%a’@." (1+i)
            pp_token_array verbatim
            pp_token_array expanded
        done
      end;
      arg_tab, macro_body
  in
  let rec replace_ws ws = function
    | Verbatim t -> Verbatim { t with ws }
    | Param (_, i) -> Param (ws, i)
    | Stringify (_, i) -> Stringify (ws, i)
    | Concat (t1, t2) -> Concat (replace_ws ws t1, t2)
    | (Magic_FILE | Magic_LINE as t) -> t
  in
  (* remove leading whitespace from macro body *)
  let macro_body =
    match macro_body with
    | [] -> []
    | t::tt -> replace_ws tok.ws t :: tt
  in
  let result = expand arg_tab macro_body in
  if !Global.debug_preproc then begin
    Format.eprintf "‘%s%s’ => ‘%a’@." tok.ws tok.text
      pp_token_or_whitespace_array result
  end;
  rev_iter_array (unget_token_or_whitespace p) result

and expand_token_array macro_tab tok_arr =
  let eof = {
    kind = EOF;
    text = "";
    pos = Lexing.dummy_pos;
    ws = ""
  } in
  let p = make_parser (fun () -> eof) in
  rev_iter_array (unget_token p) tok_arr;
  let rec loop acc =
    match getsym_expand macro_tab p with
    | { kind = EOF; _ } -> acc
    | t -> loop (t::acc)
  in
  loop [] |> List.rev |> Array.of_list

let make_cond_expander macro_tab p =
  make_parser begin fun () ->
    match getsym_expand macro_tab p with
    | { text = "defined"; _ } as t ->
      let name =
        match getsym p with
        | { kind = LPAREN; _ } ->
          let name = expect_ident p in
          let () = expect p ")" in
          name
        | { kind = PREIDENT _; text; _ } -> text
        | { pos; _ } -> error pos "identifier expected after ‘defined’"
      in
      let text = if H.mem macro_tab name then "1" else "0" in
      { t with kind = INT_LIT text; text }
    | { kind = PREIDENT _; _ } as t ->
      let text = "0" in
      { t with kind = INT_LIT text; text }
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

let handle_define st (dir_pos : Lexing.position) p =
  skip p;
  let name = expect_ident p in
(*Printf.eprintf "%s:%d: #define %s\n" pos.pos_fname pos.pos_lnum name;*)
  let def_body =
    match p.tok with
    | { kind = LPAREN; ws = ""; _ } ->
      skip p;
      (* function-like macro *)
      let param_alist, arity, is_vararg =
        match getsym p with
        | { kind = RPAREN; _ } -> [], 0, false
        | { kind = PREIDENT _; text; _ } ->
          let rec loop m i =
            match getsym p with
            | { kind = COMMA; _ } ->
              begin match getsym p with
                | { kind = PREIDENT _; text; _ } ->
                  loop ((text, i) :: m) (i+1)
                | { kind = ELLIPSIS; _ } ->
                  expect p ")";
                  m, i, true
                | { pos; _ } -> unexpected_token pos
              end
            | { kind = RPAREN; _ } -> m, i, false
            | { pos; _ } -> unexpected_token pos
          in
          loop [text, 0] 1
        | { kind = ELLIPSIS; _ } ->
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
    | _ ->
      (* object-like macro *)
      let body = parse_macro_body [] p in
      ObjLike body
  in
  let def_loc = Loc_Source (dir_pos.pos_fname, dir_pos.pos_lnum) in
  H.replace st.macro_tab name (def_body, def_loc)

let unget_lexeme (lexbuf : Lexing.lexbuf) =
  lexbuf.lex_curr_pos <- lexbuf.lex_start_pos;
  lexbuf.lex_curr_p <- lexbuf.lex_start_p

let handle_include (st:state) p lexbuf =
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
      | "define" -> handle_define st pos p
      | "undef" ->
        skip p;
        let name = expect_ident p in
(*      let pos = st.lexbuf.lex_curr_p in
        Printf.eprintf "%s:%d: #undef %s\n" pos.pos_fname pos.pos_lnum name;*)
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
      | "error" ->
        let message_offset = String.length p.tok.ws + String.length p.tok.text in
        error pos ("#error" ^ String.sub dir message_offset (String.length dir - message_offset))
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
(*    | d -> Format.eprintf "skipping directive %s@." d *)
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
          wrap_token_no_ws DIRECTIVE st.lexbuf
        else
          error st.lexbuf.lex_curr_p "unterminated #if"
    in
    match t.kind with
    | DIRECTIVE ->
      (* this needs to be the start position of the token following '#' *)
      let dir_pos = st.lexbuf.lex_curr_p in
      Lexer.directive dir_buf st.lexbuf;
(*    Format.eprintf "current position: %a@." pp_pos st.lexbuf.lex_curr_p; *)
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

let make_supplier conf inchan filename =
  let st = init_state conf inchan filename in
  let p = make_preproc_parser st in
  fun () ->
    let token = getsym_expand st.macro_tab p in
    if !Global.debug_preproc then
      Format.eprintf "[preproc] %a: %a ‘%s’ ‘%s’@."
        pp_pos token.pos Token.pp_token token.kind token.ws token.text;
    { token with kind = convert_token token.kind token.text }
