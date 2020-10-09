module P = Parser

open Dump_AST

let config =
  AST.{
    short_size = 2;
    int_size = 4;
    long_size = 4;
    long_long_size = 8;
    word_size = Type_Size.Size_Long
  }

let parse_c_file preproc_conf ic filename =
  (* why do I have to specify filename twice?? *)
  let p = P.make_parser' config (Preproc.make_supplier preproc_conf ic filename) filename in
  let result = P.parse_translation_unit p in
  P.get_messages p |> List.iter begin fun (loc, msgtype, msg) ->
    let msgtype_name =
      match msgtype with
      | P.Error -> "error"
      | Warning -> "warning"
    in
    Format.eprintf "%a: %s: %s@." AST.pp_loc loc msgtype_name msg
  end;
  result

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
    sys_include_dirs = List.rev !sys_inc_rev;
    user_include_dirs = List.rev !user_inc_rev;
  } in
  try
    let chan, filename =
      match input_file with
      | None -> (stdin, "<stdin>")
      | Some path -> (open_in path, path)
    in
    let tu = parse_c_file preproc_conf chan filename in
    if input_file <> None then close_in chan;
    tu |> List.iter (Format.printf "%a@." AST.pp_extern_decl);
    Format.printf "%a@." dump_tu tu
  with P.Syntax_Error tok ->
    Format.eprintf "%a: syntax error at ‘%s’@."
      Preproc.pp_pos tok.pos tok.text
