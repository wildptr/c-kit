module P = Parser

let config =
  Program.{
    short_size = 2;
    int_size = 4;
    long_size = 4;
    long_long_size = 8;
    word_size = AST_Types.Size_Long
  }

let parse_c_file ic filename =
  let p = P.make_parser config ic filename in
  let result = P.parse_translation_unit p in
  P.get_messages p |> List.iter begin fun (loc, msgtype, msg) ->
    let msgtype_name =
      match msgtype with
      | P.Error -> "error"
      | Warning -> "warning"
    in
    Format.eprintf "%a: %s: %s@." Program.pp_loc loc msgtype_name msg
  end;
  result

let () =
  try
    let tu = parse_c_file stdin "<stdin>" in
    tu |> List.iter (Format.printf "%a@." Program.pp_extern_decl)
  with P.Syntax_Error pos ->
    Format.eprintf "%a: syntax error@." Preproc.pp_pos pos
