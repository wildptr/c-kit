open Util

type parser_config =
  { preproc_config: Preproc.config;
    typenames: string list }

let parse_c_file conf ic filename =
  (* let supplier = Preproc.make_supplier conf.preproc_config ic filename in *)
  let pp_conf = conf.preproc_config in
  let pp_state = Preproc.init_state pp_conf ic filename in
  let pp_parser = Preproc.make_preproc_parser pp_state in
  let module C = Context.Make() in
  let module P = Parser.Make(C) in
  C.initialize_typename_table conf.typenames;
  let current_token =
    ref Preproc.{ kind = EOF; text=""; pos = Lexing.dummy_pos; ws="" }
  in
  let menhir_supplier () =
    let tok:Preproc.token' =
      let pretok = Preproc.getsym_expand pp_state.macro_tab pp_parser in
(*    if pp_conf.debug then
        Format.printf "%a: %a ‘%s’@."
          Preproc.pp_pos pretok.pos Token.pp_token pretok.kind pretok.text;*)
      let tok = { pretok with kind = Token.convert_token pretok.kind pretok.text } in
      (* recognize typedef names *)
      match tok with
      | { kind = IDENT name; _ } ->
        if C.is_typename name then
          { tok with kind = TYPEIDENT name } else tok
      | _ -> tok
    in
    current_token := tok;
    let pos0 = tok.pos in
    let pos1 = { pos0 with Lexing.pos_cnum = pos0.pos_cnum + String.length tok.text } in
    (tok.kind, pos0, pos1)
  in
  let init_pos =
    Lexing.
      { pos_fname = filename;
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0 }
  in
  let init_checkpoint = P.Incremental.translation_unit init_pos in
  let handle_error = function
    | P.MenhirInterpreter.HandlingError env ->

      prerr_endline "macros:";
      String_Table.iter begin fun macro_name def ->
        Format.eprintf "%s => ‘%a’@." macro_name
          Preproc.pp_macro_body (Preproc.macro_def_body def);
        match Preproc.macro_def_loc def with
        | Preproc.Loc_Builtin ->
          prerr_endline "  predefined";
        | Preproc.Loc_Source (fname, line) ->
          Printf.eprintf "  defined at %s:%d\n" fname line
      end pp_state.macro_tab;

      prerr_endline "typedef names:";
      String_Set.iter prerr_endline (C.all_typenames ());

      let tok = !current_token in
      let pos = tok.pos in

      Printf.eprintf "%s:%d:%d: syntax error at ‘%s’, in state %d\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
        tok.text (P.MenhirInterpreter.current_state_number env)

    | _ -> failwith "?"
  in
  P.MenhirInterpreter.loop_handle ignore handle_error menhir_supplier init_checkpoint

let append_slash_if_needed path =
  let len = String.length path in
  if len = 0 || path.[len-1] = '/' then path else path^"/"

let () =
  let sys_inc_rev = ref []
  and user_inc_rev = ref []
  and input_file = ref None
  and debug = ref false in

  let argc = Array.length Sys.argv in
  let rec parse_argv i =
    if i = argc then i
    else
      let opt = Sys.argv.(i) in
      if String.length opt > 1 && opt.[0] = '-' then
        match opt with
        | "-isystem" ->
          let arg = Sys.argv.(i+1) in
          sys_inc_rev := arg :: !sys_inc_rev;
          parse_argv (i+2)
        | "-I" ->
          let arg = Sys.argv.(i+1) in
          user_inc_rev := arg :: !user_inc_rev;
          parse_argv (i+2)
        | "-debug" ->
          debug := true;
          parse_argv (i+1)
        | _ ->
          parse_argv (i+1)
      else i
  in

  let optind = parse_argv 1 in
  if optind < argc then input_file := Some (Sys.argv.(optind));
  let input_file = !input_file in
  let preproc_config : Preproc.config = {
    sys_include_dirs =
      List.rev !sys_inc_rev |> List.map append_slash_if_needed;
    user_include_dirs =
      List.rev !user_inc_rev |> List.map append_slash_if_needed;
    debug = !debug
  } in

  let chan, filename =
    match input_file with
    | None -> (stdin, "<stdin>")
    | Some path -> (open_in path, path)
  in
  let conf =
    { preproc_config; typenames = [] }
  in
  let _ = parse_c_file conf chan filename in
  if input_file <> None then close_in chan
