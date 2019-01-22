module P = Parser.Incremental

let parse_c_file ic filename =
  let supplier = Preproc.make_supplier ic filename in
  let init_pos =
    Lexing.{
      pos_fname = filename;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    }
  in
  let init_checkpoint = P.translation_unit init_pos in
  Parser.MenhirInterpreter.loop supplier init_checkpoint

let () =
  let ast = parse_c_file stdin "<stdin>" in
  let prog = Check_AST.check_ast ast in
  prog |> List.iter (Format.printf "%a@." Program.pp_extern_decl)
