open AST
open Type_Size
open Format

let pp_escaped_string f s =
  pp_print_char f '"';
  String.iter begin function
    | '\\' -> pp_print_string f "\\\\" (* print 2 backslashes *)
    | '"' -> pp_print_string f "\\\"" (* print backslash followed by double quote *)
    | c when c>=' ' && c<='~' -> pp_print_char f c
    | c -> fprintf f "\\x%02x" (int_of_char c)
  end s;
  pp_print_char f '"'

let dump_lit f = function
  | IntLit s ->
    fprintf f "(Int %a)" pp_escaped_string s
  | FloatLit s ->
    fprintf f "(Float %a)" pp_escaped_string s
  | CharLit s ->
    fprintf f "(Char %a)" pp_escaped_string s
  | StringLit s ->
    fprintf f "(String %a)" pp_escaped_string s

let dump_list pp f = function
  | [] -> pp_print_string f "[]"
  | [x] -> fprintf f "[%a]" pp x
  | x::xs ->
    fprintf f "[%a" pp x;
    List.iter (fprintf f ", %a" pp) xs;
    pp_print_char f ']'

let dump_option pp f = function
  | None -> pp_print_string f "None"
  | Some x -> fprintf f "(Some %a)" pp x

let dump_cv f (c,v) =
  fprintf f "(%b, %b)" c v

let rec dump_type f = function
  | Void ->
    pp_print_string f "Void"
  | Int (size, signed) ->
    fprintf f "(Int %s %b)" (show_int_size size) signed
  | Float size ->
    fprintf f "(Float %s)" (show_float_size size)
  | Ptr (t, cv) ->
    fprintf f "(Ptr %a %a)" dump_type t dump_cv cv
  | Func { return_type; param_types_opt } ->
    fprintf f "(Func %a %a)" dump_type return_type
      (dump_option dump_param_type_list) param_types_opt
  | Array (t, cv, n) ->
    fprintf f "(Array %a %a %d)" dump_type t dump_cv cv n
  | Incomplete_Array (t, cv) ->
    fprintf f "(Incomplete_Array %a %a)" dump_type t dump_cv cv
  | Bool ->
    pp_print_string f "Bool"
  | Struct (i, _) ->
    fprintf f "(Struct %d)" i
  | Union (i, _) ->
    fprintf f "(Union %d)" i

and dump_param_type_list f (types, va) =
  fprintf f "(%a, %b)" (dump_list dump_type) types va

let rec dump_expr f node =
  match node.nodeval.e_kind with
  | E_Ident (name, None) ->
    failwith ("dump_expr: undeclared variable " ^ name)
  | E_Ident (_, Some obj) ->
    fprintf f "(Ident %s %d)" (show_scope obj.scope) obj.id
  | E_Lit lit ->
    fprintf f "(Lit %a)" dump_lit lit
  | E_Call (e, es) ->
    fprintf f "(Call %a %a)" dump_expr e (dump_list dump_expr) es
  | E_Unary (_, None, _) -> failwith "dump_expr: untyped E_Unary"
  | E_Unary (_, Some op', e) ->
    fprintf f "(Unary %s %a)" (show_unary_op' op') dump_expr e
  | E_IncDec (op, e) ->
    fprintf f "(IncDec %s %a)" (show_inc_dec_op op) dump_expr e
  | E_Addr e ->
    fprintf f "(Addr %a)" dump_expr e
  | E_Binary (_, None, _, _) -> failwith "dump_expr: untyped E_Binary"
  | E_Binary (_, Some op', e1, e2) ->
    fprintf f "(Binary %s %a %a)" (show_binary_op' op')
      dump_expr e1 dump_expr e2
  | E_Assign (e1, e2) ->
    fprintf f "(Assign %a %a)" dump_expr e1 dump_expr e2
  | E_Seq (e1, e2) ->
    fprintf f "(Seq %a %a)" dump_expr e1 dump_expr e2
  | E_Cond (e1, e2, e3) ->
    fprintf f "(Cond %a %a %a)" dump_expr e1 dump_expr e2 dump_expr e3
  | E_SizeOfE e ->
    fprintf f "(SizeOfE %a)" dump_expr e
  | E_SizeOfT t ->
    fprintf f "(SizeOfT %a)" dump_type t
  | E_Cast (t, e) ->
    fprintf f "(Cast %a %a)" dump_type t dump_expr e
  | E_Deref (e, cv) ->
    fprintf f "(Deref %a %a)" dump_expr e dump_cv cv
  | E_Dot (_, _, None, _) -> failwith "dump_expr: untyped E_Dot"
  | E_Dot (e, _, Some i, cv) ->
    fprintf f "(Dot %a %d %a)" dump_expr e i dump_cv cv
  | E_Arrow (_, _, None, _) -> failwith "dump_expr: untyped E_Arrow"
  | E_Arrow (e, _, Some i, cv) ->
    fprintf f "(Arrow %a %d %a)" dump_expr e i dump_cv cv
  | E_Index (e1, e2, cv) ->
    fprintf f "(Index %a %a %a)" dump_expr e1 dump_expr e2 dump_cv cv

let dump_label f = function
  | Ordinary_Label name ->
    fprintf f "(Ordinary %a)" pp_escaped_string name
  | Case_Label e ->
    fprintf f "(Case %a)" dump_expr e
  | Default_Label ->
    pp_print_string f "Default"

let rec dump_init f = function
  | Init_Expr e ->
    fprintf f "(Expr %a)" dump_expr e
  | Init_List il_node ->
    fprintf f "(Init %a)" (dump_list dump_init) il_node.nodeval

let dump_init_decl f = function
  | (_, None) -> ()
  | (d, Some ((obj:obj), init)) ->
    fprintf f "(Init %s %d %a)" (show_scope obj.scope) obj.id dump_init init

let rec dump_stmt f node =
  match node.nodeval with
  | S_Null ->
    pp_print_string f "Skip"
  | S_Label (label, s) ->
    fprintf f "(Label %a %a)" dump_label label dump_stmt s
  | S_Comp b ->
    dump_list dump_block_item f b
  | S_Expr e ->
    fprintf f "(Expr %a)" dump_expr e
  | S_If (e, s) ->
    fprintf f "(If %a %a)" dump_expr e dump_stmt s
  | S_IfElse (e, s1, s2) ->
    fprintf f "(IfElse %a %a %a)" dump_expr e dump_stmt s1 dump_stmt s2
  | S_Switch (e, s) ->
    fprintf f "(Switch %a %a)" dump_expr e dump_stmt s
  | S_While (e, s) ->
    fprintf f "(While %a %a)" dump_expr e dump_stmt s
  | S_DoWhile (s, e) ->
    fprintf f "(DoWhile %a %a)" dump_stmt s dump_expr e
  | S_For1 (e1O, e2O, e3O, s) ->
    fprintf f "(For1 %a %a %a %a)"
      (dump_option dump_expr) e1O
      (dump_option dump_expr) e2O
      (dump_option dump_expr) e3O
      dump_stmt s
  | S_For2 (_, e1O, e2O, s) ->
    fprintf f "(For2 %a %a %a)"
      (dump_option dump_expr) e1O
      (dump_option dump_expr) e2O
      dump_stmt s
  | S_Goto name ->
    fprintf f "(Goto %a)" pp_escaped_string name
  | S_Continue ->
    pp_print_string f "Continue"
  | S_Break ->
    pp_print_string f "Break"
  | S_Return eO ->
    fprintf f "(Return %a)" (dump_option dump_expr) eO

and dump_block_item f = function
  | Item_Decl idecls ->
    fprintf f "(Init %a)" (dump_list dump_init_decl) idecls
  | Item_Stmt s ->
    fprintf f "(Stmt %a)" dump_stmt s

let dump_obj f (obj:obj) =
  fprintf f "{id=%d, name=%a, type=%a, storage=%a, cv=%a}" obj.id
    pp_escaped_string obj.name
    dump_type obj.typ
    (dump_option pp_storage) obj.storage
    dump_cv obj.cv

let dump_extern_decl f = function
  | Func_Def (_,  b, objs) ->
    fprintf f "(Func_Def %a %a)" (dump_list dump_obj) objs
      (dump_list dump_block_item) b
  | Decl idecls ->
    fprintf f "(Init %a)" (dump_list dump_init_decl) idecls

let dump_tu f tu = dump_list dump_extern_decl f tu
