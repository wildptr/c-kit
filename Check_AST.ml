open Program
open AST
open ExtLib

type spec = {
  mutable base : spec_qual option;
  mutable storage : storage option;
  mutable short : bool;
  mutable long : int;
  mutable signed : bool;
  mutable unsigned : bool;
  mutable const : bool
}

let new_spec () =
  { base = None;
    storage = None;
    short = false;
    long = 0;
    signed = false;
    unsigned = false;
    const = false }

let parse_spec_list t l =
  let set_base b =
    match t.base with
    | None -> t.base <- Some b
    | Some _ -> failwith "parse_spec_list (set_base)"
  in
  let set_storage s =
    match t.storage with
    | None -> t.storage <- Some s
    | Some _ -> failwith "parse_spec_list (set_storage)"
  in
  let rec loop = function
    | [] -> ()
    | hd :: tl ->
      begin match hd with
        | S_Void | S_Char | S_Bool | S_Int | S_Float | S_Double -> set_base hd
        | S_Short -> t.short <- true
        | S_Long -> t.long <- t.long + 1
        | S_Signed -> t.signed <- true
        | S_Unsigned -> t.unsigned <- true
        | S_Typedef -> set_storage Typedef
        | S_Extern -> set_storage Extern
        | S_Static -> set_storage Static
      end;
      loop tl
  in
  loop l

let type_of_spec t =
  let check cond =
    if not cond then failwith "type_of_spec: check failed"
  in
  let base =
    match t.base with Some b -> b | None -> failwith "type_of_spec"
  in
  match base with
  | S_Void ->
    check (not t.short && t.long = 0 && not t.signed && not t.unsigned);
    Void
  | S_Char ->
    check (not t.short && t.long = 0 && not (t.signed && t.unsigned));
    (Int (Size_Char, not t.unsigned))
  | S_Bool ->
    failwith "don't know what to do with Bool"
  | S_Int ->
    check (not (t.short && t.long > 0) && not (t.signed && t.unsigned));
    let size =
      if t.short then Size_Short else
        match t.long with
        | 0 -> Size_Int
        | 1 -> Size_Long
        | 2 -> Size_Long_Long
        | _ -> failwith "too many 'long's"
    in
    (Int (size, not t.unsigned))
  | S_Float
  | S_Double ->
    failwith "don't know what do do with Float/Double"
  | _ -> assert false

let parse_cv s =
  let t = new_spec () in
  parse_spec_list t s;
  t.const

(* returns: (type_transformer, name) *)
let rec parse_declarator d =
  match d with
  | IdentDeclarator name ->
    (fun typ -> typ), name
  | ArrayDeclarator _ ->
    failwith "parse_declarator: ArrayDeclarator"
  | FuncDeclarator (d, _) ->
    (* TODO: parameter list *)
    let tt, name = parse_declarator d in
    (fun typ ->
       let func_type = {
         return_type = tt typ;
         param_types = []
       } in
       Func func_type), name
  | OldFuncDeclarator _ ->
    failwith "parse_declarator: OldFuncDeclarator"
  | PtrDeclarator (s, d') ->
    let cv = parse_cv s in
    let t, name = parse_declarator d' in
    (fun typ -> Ptr (typ, cv)), name

module H =
  Hashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end)

type env = {
  obj_table : var H.t;
  mutable ident_stack : string Stack.t list
}

let new_env () = {
  obj_table = H.create 256;
  ident_stack = [Stack.create ()]
}

let lookup_var env name =
  H.find env.obj_table name

let install_var env (var : var) =
  let name = var.name in
  H.add env.obj_table name var;
  Stack.push name (List.hd env.ident_stack)

let push_scope env =
  env.ident_stack <- Stack.create () :: env.ident_stack

let pop_scope env =
  match env.ident_stack with
  | [] -> assert false
  | hd :: tl ->
    (* remove all identifiers in current scope from symbol table *)
    hd |> Stack.iter (H.remove env.obj_table);
    env.ident_stack <- tl

let within_subscope env f =
  push_scope env;
  let result = f env in
  pop_scope env;
  result

let parse_base_type s =
  let spec = new_spec () in
  parse_spec_list spec s;
  type_of_spec spec

let parse_one_decl (s, dr) =
  let base_typ = parse_base_type s in
  let tt, name = parse_declarator dr in
  tt base_typ, name

let parse_type x =
  fst (parse_one_decl x)

let rec parse_expr env = function
  | S_IdentExpr name ->
    VarExpr (lookup_var env name)
  | S_LitExpr lit ->
    LitExpr lit
  | S_CallExpr (e, es) ->
    CallExpr (parse_expr env e, List.map (parse_expr env) es)
  | S_UnaryExpr (op, e) ->
    UnaryExpr (op, parse_expr env e)
  | S_BinaryExpr (op, e1, e2) ->
    BinaryExpr (op, parse_expr env e1, parse_expr env e2)
  | S_CondExpr (e1, e2, e3) ->
    CondExpr (parse_expr env e1, parse_expr env e2, parse_expr env e3)
  | S_SizeOfExpr typ ->
    SizeOfExpr (parse_type typ)
  | S_CastExpr (typ, e) ->
    CastExpr (parse_type typ, parse_expr env e)

(* an AST declaration is translated into a list of Program declarations,
   since an AST declaration may contain multiple declarators *)
let parse_decl env (s, dr_list) =
  let spec = new_spec () in
  parse_spec_list spec s;
  let base_typ = type_of_spec spec in
  dr_list |> List.map begin fun (d, e) ->
    let tt, name = parse_declarator d in
    let typ = tt base_typ in
    let var = { name; typ; storage = spec.storage } in
    install_var env var;
    let init = Option.map (parse_expr env) e in
    var, init
  end

let mk_assign var e =
  BinaryExpr (Assign, VarExpr var, e)

let rec parse_stmt env = function
  | S_NullStmt ->
    NullStmt
  | S_LabelStmt (label, s) ->
    let label' =
      match label with
      | S_OrdinaryLabel l -> OrdinaryLabel l
      | S_CaseLabel e -> CaseLabel (parse_expr env e)
      | S_DefaultLabel -> DefaultLabel
    in
    LabelStmt (label', parse_stmt env s)
  | S_CompStmt items ->
    CompStmt (parse_block env items)
  | S_ExprStmt e ->
    ExprStmt (parse_expr env e)
  | S_IfStmt (e, s) ->
    IfStmt (parse_expr env e, parse_stmt env s)
  | S_IfElseStmt (e, s1, s2) ->
    IfElseStmt (parse_expr env e, parse_stmt env s1, parse_stmt env s2)
  | S_SwitchStmt (e, s) ->
    SwitchStmt (parse_expr env e, parse_stmt env s)
  | S_WhileStmt (e, s) ->
    WhileStmt (parse_expr env e, parse_stmt env s)
  | S_DoWhileStmt (s, e) ->
    DoWhileStmt (parse_stmt env s, parse_expr env e)
  | S_ForStmt1 (e1_opt, e2_opt, e3_opt, s) ->
    ForStmt
      (Option.map (parse_expr env) e1_opt,
       Option.map (parse_expr env) e2_opt,
       Option.map (parse_expr env) e3_opt,
       parse_stmt env s)
  | S_ForStmt2 (d, e2_opt, e3_opt, s) ->
    within_subscope env begin fun env ->
      let decl_list = parse_decl env d in
      let rec mk_seq acc = function
        | [] -> acc
        | hd::tl ->
          begin match acc with
            | None ->
              mk_seq (Some hd) tl
            | Some e ->
              mk_seq (Some (BinaryExpr (Seq, e, hd))) tl
          end
      in
      let init =
        decl_list |> List.filter_map begin fun (decl, init_opt) ->
          Option.map (fun init -> let var = decl in mk_assign var init) init_opt
        end |> mk_seq None
      in
      CompStmt
        { decl = List.map fst decl_list;
          body =
            [ForStmt
               (init,
                Option.map (parse_expr env) e2_opt,
                Option.map (parse_expr env) e3_opt,
                parse_stmt env s)] }
    end
  | S_GotoStmt l ->
    GotoStmt l
  | S_ContinueStmt ->
    ContinueStmt
  | S_BreakStmt ->
    BreakStmt
  | S_ReturnStmt e_opt ->
    ReturnStmt (Option.map (parse_expr env) e_opt)

and parse_block env items =
  let declq = Queue.create ()
  and stmtq = Queue.create () in
  items |> List.iter begin function
    | DeclItem s_decl ->
      let decl_list = parse_decl env s_decl in
      decl_list |> List.iter begin fun (decl, init_opt) ->
        Queue.push decl declq;
        match init_opt with
        | None -> ()
        | Some init ->
          let var = decl in
          Queue.push (ExprStmt (mk_assign var init)) stmtq
      end
    | StmtItem s_stmt ->
      let stmt = parse_stmt env s_stmt in
      Queue.push stmt stmtq
  end;
  { decl = Queue.to_seq declq |> List.of_seq;
    body = Queue.to_seq stmtq |> List.of_seq }

let check_ast extern_decls =
  let env = new_env () in
  let q = Queue.create () in
  extern_decls |> List.iter begin function
    | S_FuncDef (s, dr, _, items) ->
      (* type of the function *)
      let typ, name = parse_one_decl (s, dr) in
      let v = { name; typ; storage = None } in
      install_var env v;
      push_scope env;
      let block = parse_block env items in
      pop_scope env;
      let ftyp =
        match typ with
        | Func ft -> ft
        | _ -> failwith "function definition does not define a function"
      in
      let func_def = { name; typ = ftyp; block } in
      Queue.push (FuncDef func_def) q
    | S_Decl s_decl ->
      let decl_list = parse_decl env s_decl in
      List.iter (fun (decl, _) -> Queue.push (Decl decl) q) decl_list
        (* TODO: handle initializers *)
  end;
  Queue.to_seq q |> List.of_seq
