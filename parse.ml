module P = Parser.Incremental

let () =
  let filename = "<stdin>" in
  let supplier = Preproc.make_supplier stdin in
  let init_pos =
    Lexing.{
      pos_fname = filename;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    }
  in
  let init_checkpoint = P.translation_unit init_pos in
  let ast = Parser.MenhirInterpreter.loop supplier init_checkpoint in
  List.iter (Format.printf "%a@." AST.pp_extern_decl) ast
