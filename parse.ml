module P = Parser

(*let abs_pos (pos:Lexing.position) =
  pos.pos_bol + pos.pos_cnum*)

let parse_c_file preproc_conf ic filename =
  let supplier = Preproc.make_supplier preproc_conf ic filename in
  let menhir_supplier () =
    let tok:Preproc.token' = supplier () in
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
  P.MenhirInterpreter.loop menhir_supplier init_checkpoint

let append_slash_if_needed path =
  let len = String.length path in
  if len = 0 || path.[len-1] = '/' then path else path^"/"

let () =
  let sys_inc_rev = ref []
  and user_inc_rev = ref []
  and input_file = ref None in

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
        | _ ->
          parse_argv (i+1)
      else i
  in

  let optind = parse_argv 1 in
  if optind < argc then input_file := Some (Sys.argv.(optind));
  let input_file = !input_file in
  let preproc_conf : Preproc.config = {
    sys_include_dirs =
      List.rev !sys_inc_rev |> List.map append_slash_if_needed;
    user_include_dirs =
      List.rev !user_inc_rev |> List.map append_slash_if_needed
  } in

  let chan, filename =
    match input_file with
    | None -> (stdin, "<stdin>")
    | Some path -> (open_in path, path)
  in
  let _ = parse_c_file preproc_conf chan filename in
  if input_file <> None then close_in chan
