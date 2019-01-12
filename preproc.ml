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
  payload : pptoken;
  text : string;
  pos : int;
  ws : bool; (* has preceding whitespace? *)
  no_expand_list : string list;
}

type replace_token =
  | Verbatim of token
  | Param of int
  | Stringify of bool * int
  | Concat of replace_token * replace_token

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
  { lex_state = Lexer.init_state ();
    macro_tab = H.create 0;
    token_queue = [];
    cond_stack = [] }

let wrap_token payload lex_state lexbuf =
  let text = Lexing.lexeme lexbuf in
  let pos = lexbuf.lex_start_pos in
  let ws = lex_state.Lexer.has_whitespace in
  lex_state.Lexer.has_whitespace <- false;
  { payload; text; pos; ws; no_expand_list = [] }

let lex_raw lex_state lexbuf =
  let payload = Lexer.initial lex_state lexbuf in
  wrap_token payload lex_state lexbuf

let peek (pp_state, lexbuf) =
  let t =
    match pp_state.token_queue with
    | [] ->
      let t = lex_raw pp_state.lex_state lexbuf in
      pp_state.token_queue <- [t]; t
    | t::_ -> t
  in t.payload

let skip (pp_state, _) =
  pp_state.token_queue <- List.tl pp_state.token_queue

let get (pp_state, lexbuf) =
  match pp_state.token_queue with
  | [] -> lex_raw pp_state.lex_state lexbuf
  | hd::tl -> pp_state.token_queue <- tl; hd

(* for parsing preprocessor directives *)
let parse_token_list st =
  let rec loop acc =
    let t = get st in
    if t.payload = EOL then acc else loop (t::acc)
  in
  loop [] |> List.rev

let skip_to_eol st =
  let rec loop () =
    let t = get st in
    if t.payload <> EOL then loop ()
  in
  loop ()

exception Error of string

let error s = raise (Error s)

let expect_ident st =
  match (get st).payload with
  | Ident s -> s
  | _ -> error "identifier expected"

let enqueue t (pp_state, _) =
  pp_state.token_queue <- t :: pp_state.token_queue

let match_token p st =
  let t = get st in
  if t.payload = p then true else (enqueue t st; false)

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

let escape_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  s |> String.iter begin fun c ->
    if c = '"' || c = '\\' then Buffer.add_char buf '\\';
    Buffer.add_char buf c
  end;
  Buffer.add_char buf '"';
  Buffer.contents buf

let stringify ws l =
  let s =
    match l with
    | [] -> ""
    | hd::tl ->
      let buf = Buffer.create 16 in
      Buffer.add_string buf hd.text;
      tl |> List.iter begin fun t ->
        if t.ws then Buffer.add_char buf ' ';
        Buffer.add_string buf t.text
      end;
      Buffer.contents buf
  in
  { payload = StringLit s; text = escape_string s;
    pos = 0; ws = false; no_expand_list = [] }

let concat_token t1 t2 =
  let text = t1.text ^ t2.text in
  { payload = parse_token text; text;
    pos = 0; ws = false; no_expand_list = [] }

let rec parse_macro_body param_map = function
  | Verbatim { payload = Ident s; _ } :: tl
    when M.mem s param_map ->
    let i = M.find s param_map in
    parse_macro_body param_map (Param i :: tl)
  | (Verbatim { payload = Hash; ws; _ } as hd1) ::
    (Verbatim { payload = Ident s; _ } as hd2) :: tl ->
    begin match M.find s param_map with
      | i -> Stringify (ws, i) :: parse_macro_body param_map tl
      | exception Not_found -> hd1 :: hd2 :: parse_macro_body param_map tl
    end
  | hd1 :: Verbatim { payload = HashHash; _ } :: tl ->
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
    if t.ws then pp_print_char f ' ';
    pp_print_string f t.text
  end

let rec subst_token macro_name no_expand_list arg_tab = function
  | Verbatim u -> [{ u with no_expand_list = macro_name :: no_expand_list }]
  | Param i ->
    arg_tab.(i) |> List.map
      (fun u -> { u with no_expand_list = macro_name :: u.no_expand_list })
  | Stringify (ws, i) -> [stringify ws arg_tab.(i)]
  | Concat (u1, u2) ->
    let u1' = subst_token macro_name no_expand_list arg_tab u1 in
    let u2' = subst_token macro_name no_expand_list arg_tab u2 in
    begin match u1', u2' with
      | [], _ -> u2'
      | _, [] -> u1'
      | _, t2::l2 ->
        let l1, t1 =
          match List.rev u1' with
          | h::t -> List.rev t, h
          | _ -> assert false
        in
        l1 @ concat_token t1 t2 :: l2
    end

let rec lex (pp_state, lexbuf as st) =
  let t =
    match pp_state.token_queue with
    | [] ->
      begin match pp_state.cond_stack with
        | [] | (true, _) :: _ ->
          lex_raw pp_state.lex_state lexbuf
        | (false, false) :: _ ->
          let lex_state = pp_state.lex_state in
          let payload = Lexer.skip_to_else_endif lex_state lexbuf in
          wrap_token payload lex_state lexbuf
        | (false, true) :: _ ->
          let lex_state = pp_state.lex_state in
          let payload = Lexer.skip_to_endif lex_state lexbuf in
          wrap_token payload lex_state lexbuf
      end
    | hd :: tl ->
      let () = pp_state.token_queue <- tl in hd
  in
  match t.payload with
  | DEFINE ->
    let name = expect_ident st in
    let def =
      match get st with
      | { payload = LParen; ws = false; _ } ->
        (* function-like macro *)
        let param_map, arity =
          match get st with
          | { payload = RParen; _ } -> M.empty, 0
          | { payload = Ident s; _ } ->
            let rec loop m i =
              match (get st).payload with
              | Comma ->
                let s = expect_ident st in
                loop (M.add s i m) (i+1)
              | RParen -> m, i
              | _ -> error "#define"
            in
            loop (M.singleton s 0) 1
          | _ -> error "#define"
        in
        let body =
          parse_macro_body param_map
            (parse_token_list st |> List.map (fun t -> Verbatim t))
        in
        FunLike (arity, body)
      | u ->
        (* object-like macro *)
        enqueue u st;
        let body =
          parse_macro_body M.empty
            (parse_token_list st |> List.map (fun t -> Verbatim t))
        in
        ObjLike body
    in
    H.replace pp_state.macro_tab name def;
    lex st
  | UNDEF ->
    let name = expect_ident st in
    H.remove pp_state.macro_tab name;
    lex st
  | Ident s
    when not (List.mem s t.no_expand_list) && H.mem pp_state.macro_tab s ->
    let expand arg_tab body =
      let l' =
        body |> List.map (subst_token s t.no_expand_list arg_tab) |> List.concat
      in
      begin match l' with
        | [] -> ()
        | hd::tl ->
          pp_state.token_queue <-
            { hd with ws = t.ws } :: tl @ pp_state.token_queue
      end;
      lex st
    in
    begin match H.find pp_state.macro_tab s with
      | ObjLike body -> expand [||] body
      | FunLike (arity, body) ->
        if match_token LParen st then
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
        else t
    end
  | IFDEF ->
    begin match parse_token_list st with
      | [{ payload = Ident name; _ }] ->
        pp_state.cond_stack <-
          (H.mem pp_state.macro_tab name, false) :: pp_state.cond_stack
      | _ -> error "syntax error in #ifdef"
    end;
    lex st
  | IFNDEF ->
    begin match parse_token_list st with
      | [{ payload = Ident name; _ }] ->
        pp_state.cond_stack <-
          (not (H.mem pp_state.macro_tab name), false) :: pp_state.cond_stack
      | _ -> error "syntax error in #ifndef"
    end;
    lex st
  | ELSE ->
    skip_to_eol st;
    begin match pp_state.cond_stack with
      | [] -> error "#else without #if"
      | (_, true) :: _ -> error "duplicate #else"
      | (cond, false) :: tl -> pp_state.cond_stack <- (not cond, true) :: tl
    end;
    lex st
  | ENDIF ->
    skip_to_eol st;
    begin match pp_state.cond_stack with
      | [] -> error "#endif without #if"
      | _::tl -> pp_state.cond_stack <- tl
    end;
    lex st
  | _ -> t

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
    if t.payload = EOF then acc else loop (t::acc)
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
(*     Format.printf "%d: %a ‘%s’\n" token.pos pp_pptoken token.payload token.text; *)
    let start_pos = lexbuf.lex_start_p in
    let end_pos =
      { start_pos with
        pos_cnum = start_pos.pos_cnum + String.length token.text }
    in
    let token' = convert_token token.payload in
    Format.printf "%d: %a ‘%s’\n"
      token.pos pp_pptoken token.payload token.text;
    token', start_pos, end_pos

let main () =
  Printexc.record_backtrace true;
  let lexbuf = Lexing.from_channel stdin in
  let pp_state = init_state () in
  let rec loop () =
    let t = lex (pp_state, lexbuf) in
    Printf.printf "%d: %s ‘%s’\n" t.pos (show_pptoken t.payload) t.text;
 (* if t.ws then print_char ' ';
    print_string t.text; *)
    if t.payload = EOF then () else loop ()
  in
  loop ()
