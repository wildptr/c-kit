module P = Parser

open AST
open Dump_AST

let config =
  AST.{
    short_size = 2;
    int_size = 4;
    long_size = 4;
    long_long_size = 8;
    word_size = Type_Size.Size_Long
  }

let parse_c_file ic filename =
  (* why do I have to specify filename twice?? *)
  let p = P.make_parser' config (Preproc.make_supplier ic filename) filename in
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
  try
    let tu = parse_c_file stdin "<stdin>" in
    tu |> List.iter (Format.printf "%a@." AST.pp_extern_decl);
    Format.printf "%a@." dump_tu tu
  with P.Syntax_Error tok ->
    Format.eprintf "%a: syntax error at ‘%s’@."
      Preproc.pp_pos tok.pos tok.text
