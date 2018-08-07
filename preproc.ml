open Batteries
open Token

type token = {
  payload : Token.token;
  text : string;
  pos : int;
  ws : bool;
  no_expand_list : string list;
}

type replace_token =
  | Verbatim of token
  | Param of int
  | Stringify of bool * int
  | Concat of replace_token * replace_token

type macro_def =
  | ObjLike of replace_token list
  | FunLike of replace_token list

type state = {
  lexbuf : Lexing.lexbuf;
  lex_state : Lexer.state;
  macro_tab : (string, macro_def) Hashtbl.t;
  mutable token_queue : token list
}

let init_state lexbuf =
  { lexbuf;
    lex_state = Lexer.init_state ();
    macro_tab = Hashtbl.create 0;
    token_queue = [] }

let lex_raw st =
  let payload = Lexer.initial st.lex_state st.lexbuf in
  let text = Lexing.lexeme st.lexbuf in
  let pos = st.lexbuf.lex_start_pos in
  let ws = st.lex_state.has_whitespace in
  st.lex_state.has_whitespace <- false;
  { payload; text; pos; ws; no_expand_list = [] }

let peek st =
  let t =
    match st.token_queue with
    | [] ->
      let t = lex_raw st in
      (st.token_queue <- [t]; t)
    | t::_ -> t
  in t.payload

let skip st =
  st.token_queue <- List.tl st.token_queue

let get st =
  match st.token_queue with
  | [] -> lex_raw st
  | hd::tl -> st.token_queue <- tl; hd

let parse_token_list st =
  let rec loop acc =
    let t = get st in
    if t.payload = EOL then acc else loop (t::acc)
  in
  loop [] |> List.rev

exception Error of string

let error s = raise (Error s)

let expect_ident st =
  match (get st).payload with
  | Ident s -> s
  | _ -> error "identifier expected"

let enqueue t st =
  st.token_queue <- t :: st.token_queue

let match_token p st =
  let t = get st in
  if t.payload = p then true else (enqueue t st; false)

let parse_macro_arg st =
  let rec loop depth acc =
    match peek st with
    | Comma -> if depth = 0 then acc else loop depth (get st :: acc)
    | LParen -> loop (depth+1) (get st :: acc)
    | RParen -> if depth = 0 then acc else loop (depth-1) (get st :: acc)
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
    when Map.String.mem s param_map ->
    let i = Map.String.find s param_map in
    parse_macro_body param_map (Param i :: tl)
  | (Verbatim { payload = Hash; ws; _ } as hd1) ::
    (Verbatim { payload = Ident s; _ } as hd2) :: tl ->
    begin match Map.String.Exceptionless.find s param_map with
      | Some i -> Stringify (ws, i) :: parse_macro_body param_map tl
      | None -> hd1 :: hd2 :: parse_macro_body param_map tl
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

let rec lex st =
  let t = get st in
  match t.payload with
  | DEFINE ->
    let name = expect_ident st in
    let def =
      match get st with
      | { payload = LParen; ws = false; _ } ->
        (* function-like macro *)
        let param_map =
          match get st with
          | { payload = RParen; _ } -> Map.String.empty
          | { payload = Ident s; _ } ->
            let rec loop m i =
              match (get st).payload with
              | Comma ->
                let s = expect_ident st in
                loop (Map.String.add s i m) (i+1)
              | RParen -> m
              | _ -> error "#define"
            in
            loop (Map.String.singleton s 0) 1
          | _ -> error "#define"
        in
        let body =
          parse_macro_body param_map
            (parse_token_list st |> List.map (fun t -> Verbatim t))
        in
        FunLike body
      | u ->
        (* object-like macro *)
        enqueue u st;
        let body =
          parse_macro_body Map.String.empty
            (parse_token_list st |> List.map (fun t -> Verbatim t))
        in
        ObjLike body
    in
    Hashtbl.replace st.macro_tab name def;
    lex st
  | UNDEF ->
    let name = expect_ident st in
    Hashtbl.remove st.macro_tab name;
    lex st
  | Ident s
    when not (List.mem s t.no_expand_list) && Hashtbl.mem st.macro_tab s ->
    let expand arg_tab body =
      let l' =
        body |> List.map (subst_token s t.no_expand_list arg_tab) |>
        List.concat |> expand_token_list st.macro_tab
      in
      begin match l' with
        | [] -> ()
        | hd::tl ->
          st.token_queue <- { hd with ws = t.ws } :: tl @ st.token_queue;
      end;
      lex st
    in
    begin match Hashtbl.find st.macro_tab s with
      | ObjLike body -> expand [||] body
      | FunLike body ->
        if match_token LParen st then
          let arg_tab =
            parse_macro_arg_list st |>
            List.map (expand_token_list st.macro_tab) |>
            Array.of_list
          in
          expand arg_tab body
        else t
    end
  | _ -> t

and expand_token_list macro_tab l =
  let st = {
    lexbuf = Lexing.from_string "";
    lex_state = Lexer.init_state ();
    macro_tab;
    token_queue = l;
  } in
  let rec loop acc =
    let t = lex st in
    if t.payload = EOF then acc else loop (t::acc)
  in
  loop [] |> List.rev

let () =
  Printexc.record_backtrace true;
  let lexbuf = Lexing.from_channel stdin in
  let st = init_state lexbuf in
  let rec loop () =
    let t = lex st in
    Printf.printf "%d: %s ‘%s’\n" t.pos (show_token t.payload) t.text;
    if t.payload = EOF then () else loop ()
  in
  loop ()
