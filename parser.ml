(* This parser does not explicitly build an abstract syntax tree.  Rather,
   it does some basic semantic analysis and builds a somewhat higher-level
   representation of the program. *)

open Token
open AST
open ExtLib
open Type_Size

module H =
  Hashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end)

type su_info = {
  name : string option;
  mutable def : struct_union_def option
}

type ident_set = {
  objects : string Stack.t;
  tags : string Stack.t;
  typedefs : string Stack.t
}

let new_ident_set () = {
  objects = Stack.create ();
  tags = Stack.create ();
  typedefs = Stack.create ()
}

type tag =
  | Tag_Struct of int
  | Tag_Union of int
  | Tag_Enum of typ

type env = {
  obj_table : obj H.t;
  tag_table : tag H.t;
  typedef_table : (typ * cv) H.t;
  mutable ident_stack : ident_set list;
  su_table : su_info DynArray.t;
  mutable global_id : int;
  mutable local_id : int;
  mutable global_objects_rev : obj list;
  mutable local_objects_rev : obj list
}

let new_env () = {
  obj_table = H.create 256;
  tag_table = H.create 64;
  typedef_table = H.create 64;
  ident_stack = [new_ident_set ()];
  su_table = DynArray.create ();
  global_id = 0;
  local_id = 0;
  global_objects_rev = [];
  local_objects_rev = []
}

let is_in_global_scope env =
  List.tl env.ident_stack = []

let gen_id env =
  if is_in_global_scope env then (Global, let i = env.global_id in env.global_id <- i+1; i)
  else (Local, let i = env.local_id in env.local_id <- i+1; i)

let is_typedef_name env name =
  H.mem env.typedef_table name

let get_typedef_type env name =
  H.find env.typedef_table name

let get_real_int_size conf = function
  | Size_Char -> 1
  | Size_Short -> conf.short_size
  | Size_Int -> conf.int_size
  | Size_Long -> conf.long_size
  | Size_Long_Long -> conf.long_long_size

let ptr_size conf =
  get_real_int_size conf conf.word_size

let align_up size align =
  (size + (align-1)) land (-align)

let get_struct_union_def env id =
  let struct_info = DynArray.get env.su_table id in
  Option.get struct_info.def

(* make sure argument is object type before calling *)
let rec size_of_type conf env = function
  | Bool -> 1
  | Int (size, _) ->
    get_real_int_size conf size
  | Float size ->
    (* TODO: I'm not sure this is always the case *)
    begin match size with
      | Size_Float -> 4
      | Size_Double -> 8
      | Size_Long_Double -> 8 (* hmm... *)
    end
  | Ptr _ -> ptr_size conf
  | Array (typ, _, n) ->
    n * align_up (size_of_type conf env typ) (align_of_type conf env typ)
  | Struct (id, _) ->
    let fields = get_struct_union_def env id in
    Array.fold_left begin fun size decl ->
      let typ = decl.d_type in
      align_up size (align_of_type conf env typ) + size_of_type conf env typ
    end 0 fields
  | Union (id, _) ->
    let fields = get_struct_union_def env id in
    Array.fold_left
      (fun size decl -> max size (size_of_type conf env decl.d_type))
      0 fields
  | _ -> assert false

(* make sure argument is object type before calling *)
and align_of_type conf env = function
  | Bool | Int _ | Float _ | Ptr _ as typ -> size_of_type conf env typ
  | Struct (id, _) | Union (id, _) ->
    let fields = get_struct_union_def env id in
    Array.fold_left
      (fun align decl -> max align (align_of_type conf env decl.d_type))
      0 fields
  | _ -> assert false

type msgtype = Error | Warning

let print_message (loc, msgtype, msg) =
  let msgtype_name =
    match msgtype with
    | Error -> "error"
    | Warning -> "warning"
  in
  Format.eprintf "%a: %s: %s@." pp_loc loc msgtype_name msg

let token_loc tok = (tok.Preproc.pos, Preproc.token_end_pos tok)

type parser = {
  supplier : unit -> Preproc.token';
  mutable tok : Preproc.token';
(*
  mutable pos : Lexing.position; (* start of current lookahead token *)
  mutable last_end : Lexing.position; (* end of last consumed token *)
  mutable pos_end : Lexing.position; (* end of current lookahead token *)
*)
  mutable tokq : Preproc.token' list;
  mutable current_node_tokseq : token_seq;
  mutable tokseq_stack : token_seq list;
  env : env;
  messages : (loc * msgtype * string) Stack.t;
  config : config
}

let last_end p =
  match p.current_node_tokseq with
  | Empty (_, pos) -> pos
  | Nonempty r -> token_rope_end r

let lookup_obj env name =
  H.find env.obj_table name

let declare_object env (obj : obj) =
  let name = obj.name in
  H.add env.obj_table name obj;
  let idents = List.hd env.ident_stack in
  Stack.push name idents.objects;
  match obj.scope with
  | Global -> env.global_objects_rev <- obj :: env.global_objects_rev
  | Local -> env.local_objects_rev <- obj :: env.local_objects_rev

let declare_tag env name tag =
  H.add env.tag_table name tag;
  let idents = List.hd env.ident_stack in
  Stack.push name idents.tags

let declare_typedef env name qtyp =
  H.add env.typedef_table name qtyp;
  let idents = List.hd env.ident_stack in
  Stack.push name idents.typedefs

let define_constant env name typ value =
  let (scope, id) = gen_id env in
  let obj = {
    name;
    typ;
    storage = None; (* TODO: or rather Constant? *)
    value = Some value;
    cv = no_cv;
    scope; id
  } in
  declare_object env obj

let push_scope env =
  env.ident_stack <- new_ident_set () :: env.ident_stack

let pop_scope env =
  match env.ident_stack with
  | [] -> assert false
  | { objects; tags; typedefs } :: tl ->
    (* remove all identifiers in current scope from symbol table *)
    objects |> Stack.iter (H.remove env.tag_table);
    tags |> Stack.iter (H.remove env.tag_table);
    typedefs |> Stack.iter (H.remove env.typedef_table);
    env.ident_stack <- tl

let declare env (d : decl) =
  if d.d_storage = Some Typedef then begin
    Format.eprintf "%a: type %s = %a@."
      pp_loc (token_seq_loc d.d_declarator.tokens) d.d_name
      pp_typ_cv (d.d_type, d.d_cv);
    declare_typedef env d.d_name (d.d_type, d.d_cv)
  end else declare_object env (obj_of_decl (gen_id env) d)

let make_parser' config supplier filename =
  let tok = supplier () in
  { supplier;
    tok;
    tokq = [];
    current_node_tokseq = Empty (Lexing.dummy_pos, Lexing.dummy_pos);
    tokseq_stack = [];
    env = new_env ();
    messages = Stack.create ();
    config }

let make_parser config ic filename =
  let supplier = Preproc.make_supplier ic filename in
  make_parser' config supplier filename

(*let make_supplier_from_string s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = "<input>" };
  let ws_buf = Buffer.create 0 in
  fun () ->
    let token = Lexer.token ws_buf lexbuf in
    let text = Lexing.lexeme lexbuf in
    let pos = Lexing.(lexbuf.lex_start_p)
    and pos_end = Lexing.(lexbuf.lex_curr_p) in
    Preproc.(convert_token token text), pos, pos_end

let make_parser_s config s =
  let supplier = make_supplier_from_string s in
  make_parser' config supplier "<input>"*)

let curtok p = p.tok.kind

let skip p =
  let tok' =
    match p.tokq with
    | hd::tl -> p.tokq <- tl; hd
    | [] -> p.supplier ()
  in
  p.current_node_tokseq <-
    join_token_seq p.current_node_tokseq (Nonempty (One p.tok));
  (*Format.printf ">>> ";
  p.tokseq_stack |> List.rev |> List.iter (Format.printf "%a│" pp_token_seq);
  Format.printf "%a@." pp_token_seq p.current_node_tokseq;*)
  p.tok <- tok'

let skip' p =
  let ts = Nonempty (One p.tok) in skip p; ts

let start_node p =
  p.tokseq_stack <- p.current_node_tokseq :: p.tokseq_stack;
  p.current_node_tokseq <- Empty (p.tok.pos, p.tok.pos)

let end_node p =
  let ts = p.current_node_tokseq in
  match p.tokseq_stack with
  | hd::tl ->
    p.current_node_tokseq <- join_token_seq hd ts;
    p.tokseq_stack <- tl;
    (*Format.printf "%a@." pp_token_seq ts;*)
    ts
  | [] -> failwith "end_node"

let getsym p =
  let tok = curtok p in
  skip p;
  tok

let ungetsym p =
  match p.current_node_tokseq with
  | Nonempty (One tok) ->
    p.tokq <- p.tok :: p.tokq;
    p.tok <- tok;
    p.current_node_tokseq <- Empty (tok.pos, tok.pos)
  | Nonempty (Seq (ts, One tok)) ->
    p.tokq <- p.tok :: p.tokq;
    p.tok <- tok;
    p.current_node_tokseq <- Nonempty ts
  | _ -> failwith "ungetsym"

let error p loc msg =
  Stack.push (loc, Error, msg) p.messages

let warn p loc msg =
  Stack.push (loc, Warning, msg) p.messages

let get_messages p =
  p.messages |> Stack.to_seq |> List.of_seq |> List.rev

exception Syntax_Error of Preproc.token'

let syntax_error tok =
  raise (Syntax_Error tok)

let expect p tok =
  if curtok p <> tok then syntax_error p.tok;
  skip p

let expect_ident p =
  match curtok p with
  | Ident s -> skip p; s
  | _ -> syntax_error p.tok

let try_read_ident p =
  match curtok p with
  | Ident s -> skip p; Some s
  | _ -> None

(* Typing *)

(* 6.3.2.1  3,4 *)
let adjust_array_func_type = function
  | Array (typ, cv, _) ->
    Ptr (typ, cv)
  | Incomplete_Array (typ, cv) ->
    Ptr (typ, cv)
  | Func _ as t ->
    Ptr (t, no_cv)
  | t -> t

(* Types are automatically adjusted during parsing.  This function is called
   when the original type is needed. *)
let unadjust e =
  match e.e_kind with
  | E_Lit (StringLit s) ->
    { e with e_type_opt = Some (Array (char, mk_cv true false, String.length s + 1)) }
  | E_Ident (_, Some v) ->
    if Some v.typ = e.e_type_opt then e else
      { e with e_type_opt = Some v.typ }
  | _ -> e

let unadjust' enode =
  { enode with nodeval = unadjust enode.nodeval }

(*
let mk_conv e typ' =
  { e with e_kind = E_Conv e; e_type_opt = Some typ' }

let mk_conv_if_needed e typ' =
  match e.e_type_opt with
  | None -> e
  | Some typ -> if typ = typ' then e else mk_conv e typ'
*)

let lvalue_status = function
  | E_Ident (_, None)
  | E_Lit _
  | E_Call _
  | E_Unary _
  | E_IncDec _
  | E_Addr _
  | E_Binary _
  | E_Assign _
  | E_Seq _
  | E_Cond _
  | E_SizeOfE _
  | E_SizeOfT _
  | E_Cast _ -> None
  | E_Ident (_, Some v) -> Some v.cv
  | E_Deref (_, cv) -> Some cv
  | E_Dot (_, _, _, cv) -> Some cv
  | E_Arrow (_, _, _, cv) -> Some cv
  | E_Index (_, _, cv) -> Some cv

let get_cv e =
  Option.default no_cv (lvalue_status e.e_kind)

(* ??? *)
let can_assign_to e typ =
  let e_type = get_type e in
  match e_type with
  | Void -> false
  | Int _ ->
    begin match typ with
      | Bool | Int _ | Float _ -> true
      | Ptr _ ->
        e.e_value = Some 0L (* null pointer constant *)
      | _ -> false
    end
  | Float size ->
    begin match typ with
      | Int _ | Float _ -> true
      | _ -> false
    end
  | Ptr (t, cv) ->
    begin match typ with
      | Ptr (t', cv') ->
        t = typ && cv_le cv cv'
      | _ -> false
    end
  | Func _ | Array _ | Incomplete_Array _ -> assert false
  | Bool ->
    begin match typ with
      | Int _ | Bool -> true
      | _ -> false
    end
  | Struct _ | Union _ ->
    e_type = typ

let promote_integer_type conf = function
  | Bool ->
    int
  | Int (size, signed) as t ->
    if size < Size_Int then
      (* subject to promotion *)
      (* If an int can represent all values of the original type, the value is
         converted to an int; otherwise it is converted to an unsigned int. *)
      if signed then int else
        (* unsigned char/short *)
        let real_size = get_real_int_size conf size
        and real_size_int = get_real_int_size conf Size_Int in
        if real_size < real_size_int then int else unsigned_int
    else t (* the type is not smaller than int, so don't promote *)
  | _ -> assert false

let need_promotion = function
  | Bool -> true
  | Int (size, _) -> size < Size_Int
  | _ -> false

let find_field fields name =
  let n = Array.length fields in
  let rec loop i =
    if i < n then
      if fields.(i).d_name = name then i else loop (i+1)
    else raise Not_found
  in
  loop 0

(* *)

type spec_base_type =
  | Unspecified
  | Spec_Void
  | Spec_Bool
  | Spec_Char
  | Spec_Int
  | Spec_Float
  | Spec_Double
  | Spec_Type of typ

type spec = {
  mutable base : spec_base_type;
  mutable storage : storage option;
  mutable size : int;
  mutable sign : bool option;
  mutable const : bool;
  mutable volatile : bool
}

let make_type spec =
  (* TODO: validate *)
  match spec.base with
  | Spec_Void ->
    Void
  | Spec_Bool ->
    Bool
  | Spec_Char ->
    Int (Size_Char, Option.default true spec.sign)
  | Unspecified | Spec_Int ->
    let size =
      match spec.size with
      | -1 -> Size_Short
      | 0 -> Size_Int
      | 1 -> Size_Long
      | 2 -> Size_Long_Long
      | _ -> assert false
    in
    Int (size, Option.default true spec.sign)
  | Spec_Float ->
    Float Size_Float
  | Spec_Double ->
    let size =
      match spec.size with
      | 0 -> Size_Double
      | 1 -> Size_Long_Double
      | _ ->
        (* error *)
        Size_Double
    in
    Float size
  | Spec_Type typ ->
    typ

let make_cv spec =
  (spec.const, spec.volatile)

(* TODO: propagate location information *)
let make_decl' spec_storage spec_cv spec_typ
    ({nodeval=(d:'a declarator); tokens=_ts} as dnode) p =
  let rec loop qtyp = function
    | D_Base name -> (qtyp, name)
    | D_Array (dnode, e_opt) ->
      let (typ, cv), name = loop qtyp dnode.nodeval in
      begin match e_opt with
        | Some e ->
          begin match e.nodeval.e_value with
            | Some n ->
              ((Array (typ, cv, Int64.to_int n), no_cv), name)
            | None ->
              error p (node_loc e) "not a constant expression";
              ((Incomplete_Array (typ, cv), no_cv), name)
          end
        | None ->
          ((Incomplete_Array (typ, cv), no_cv), name)
      end
    | D_Func (dnode, l, vararg) ->
      let (typ, _), name = loop qtyp dnode.nodeval in
      let l' = List.map (fun d -> d.d_type) l in
      let func_type =
        { return_type = typ;
          param_types_opt = Some (l', vararg) }
      in
      ((Func func_type, no_cv), name)
    | D_Old_Func (dnode, _) ->
      let (typ, _), name = loop qtyp dnode.nodeval in
      let func_type =
        { return_type = typ;
          param_types_opt = None }
      in
      ((Func func_type, no_cv), name)
    | D_Ptr (dnode, cv) ->
      let (typ, cv'), name = loop qtyp dnode.nodeval in
      ((Ptr (typ, cv'), cv), name)
  in
  let (d_type, d_cv), d_name = loop (spec_typ, spec_cv) d in
  { d_storage = spec_storage;
    d_cv;
    d_type;
    d_name;
    d_declarator = dnode }

let make_decl spec d p =
  make_decl' spec.storage (make_cv spec) (make_type spec) d p

(* Parsing *)

exception Type_Error

let get_field_info su_type field_name tokseq p =
  let type_name_prefix, su_id =
    match su_type with
    | Struct (id, _) ->
      ("struct ", id)
    | Union (id, _) ->
      ("union ", id)
    | _ -> assert false
  in
  let su_table = p.env.su_table in
  let { name = tag_opt; def = def_opt } =
    DynArray.get su_table su_id
  in
  let type_name = type_name_prefix ^ Option.default "<anonymous>" tag_opt in
  match def_opt with
  | Some fields ->
    let field_id =
      try find_field fields field_name
      with Not_found ->
        error p (token_seq_loc tokseq)
          (Printf.sprintf "‘%s’ has no field named ‘%s’"
             type_name field_name);
        raise Type_Error
    in
    let d = fields.(field_id) in
    field_id, d.d_type, d.d_cv
  | None ->
    error p (token_seq_loc tokseq)
      (Printf.sprintf "access of member ‘%s’ of incomplete type ‘%s’"
         field_name type_name);
    raise Type_Error

let check_inc_dec_operand op e p =
  (* ensure e is non-const-qualified lvalue *)
  begin match lvalue_status e.nodeval.e_kind with
  | None ->
    error p (node_loc e)
      (Printf.sprintf "operand of ‘%s’ is not an lvalue" (inc_dec_op_name op))
  | Some cv ->
    if cv_is_const cv then
      error p (node_loc e)
        (Printf.sprintf "operand of ‘%s’ is const-qualified" (inc_dec_op_name op))
  end;
  (* check type of e *)
  begin match e.nodeval.e_type_opt with
    | None -> ()
    | Some typ ->
      begin match typ with
        | Int _
        | Float _
        | Ptr _ -> ()
        | Void
        | Bool
        | Struct _
        | Union _ -> error p (node_loc e) "type error"
        | Func _
        | Array _
        | Incomplete_Array _ -> assert false
      end
  end

let eval_unary_expr op value_opt =
  match value_opt with
  | None -> None
  | Some x ->
    Some begin
      match op with
      | Plus -> x
      | Minus -> Int64.neg x
      | Not -> Int64.lognot x
      | LogNot -> if x = 0L then 1L else 0L
    end

let eval_binary_expr op v1_opt v2_opt =
  match v1_opt, v2_opt with
  | Some a, Some b ->
    let result =
      match op with
      | Mul ->
        Int64.mul a b
      | Div ->
        Int64.div a b
      | Mod ->
        Int64.rem a b
      | Add ->
        Int64.add a b
      | Sub ->
        Int64.sub a b
      | LShift ->
        Int64.shift_left a (Int64.to_int b)
      | RShift ->
        Int64.shift_right a (Int64.to_int b)
      | Lt ->
        if a < b then 1L else 0L
      | Gt ->
        if a > b then 1L else 0L
      | LtEq ->
        if a <= b then 1L else 0L
      | GtEq ->
        if a >= b then 1L else 0L
      | Eq ->
        if a = b then 1L else 0L
      | NotEq ->
        if a <> b then 1L else 0L
      | And ->
        Int64.logand a b
      | Xor ->
        Int64.logxor a b
      | Or ->
        Int64.logor a b
      | LogAnd ->
        if a<>0L && b<>0L then 1L else 0L
      | LogOr ->
        if a<>0L || b<>0L then 1L else 0L
    in
    Some result
  | _ -> None

type type_kind =
  | Object_Type
  | Function_Type
  | Incomplete_Type

let rec kind_of_type env = function
  | Void | Incomplete_Array _ -> Incomplete_Type
  | Bool | Int _ | Float _ | Ptr _ -> Object_Type
  | Array (typ, _, _) ->
    begin match kind_of_type env typ with
      | Object_Type -> Object_Type
      | Function_Type -> failwith "kind_of_type: array of functions"
      | Incomplete_Type -> Incomplete_Type
    end
  | Func _ -> Function_Type
  | Struct (id, _) | Union (id, _) ->
    let su_info = DynArray.get env.su_table id in
    begin match su_info.def with
      | Some _ -> Object_Type
      | None -> Incomplete_Type
    end

let usual_arith_conv1 conf typ =
  if need_promotion typ then
    promote_integer_type conf typ
  else typ

let usual_arith_conv1' conf op typ =
  match usual_arith_conv1 conf typ with
  | Int (isize, signed) -> lower_unop_i (isize, signed) op
  | Float fsize -> lower_unop_f fsize op
  | _ -> assert false

let ensure_arith_type typ p loc =
  if not (is_arith_type typ) then
    error p loc "operand not of arithmetic type"

let ensure_integer_type typ p loc =
  if not (is_integer_type typ) then
    error p loc "operand not of integer type"

let ensure_complete_type typ p loc =
  if kind_of_type p.env typ = Incomplete_Type then
    error p loc "type of operand is incomplete"

let ensure_scalar_type typ p loc =
  if not (is_scalar_type typ) then
    error p loc "operand not of scalar type"

(*
let promote_to_word_size conf e =
  match get_type e with
  | Int (size, signed) ->
    if size < conf.word_size then
      mk_conv e (Int (conf.word_size, signed))
    else e
  | Bool ->
    mk_conv e (Int (conf.word_size, false))
  | Ptr _ -> e
  | _ -> assert false
*)

let with_type_of e1 f =
  try
    match e1.e_type_opt with
    | Some t1 -> Some (f t1)
    | _ -> raise Type_Error
  with Type_Error -> None

let with_types_of e1 e2 f =
  try
    match e1.e_type_opt, e2.e_type_opt with
    | Some t1, Some t2 -> Some (f t1 t2)
    | _ -> raise Type_Error
  with Type_Error -> None

(* returns the operand (e) after conversion and the type of the result of the
   unary operation *)
let type_unary_expr op e p : unary_op' option =
  let conf = p.config in
  with_type_of e.nodeval begin fun typ ->
    match op with
    | Plus | Minus -> (* perform promotion *)
      ensure_arith_type typ p (node_loc e);
      usual_arith_conv1' conf op typ
    | Not -> (* perform promotion *)
      ensure_integer_type typ p (node_loc e);
      usual_arith_conv1' conf op typ
    | LogNot -> (* no promotion *)
      ensure_scalar_type typ p (node_loc e);
      LogNot'
  end

let get_int_type_info = function
  | Int (size, signed) -> size, signed
  | _ -> assert false

let get_int_type_size = function
  | Int (size, _) -> size
  | _ -> assert false

let usual_arith_conv conf t1 t2 =
  match t1, t2 with
  | Float size1, Float size2 ->
    if size2 > size1 then t2 else t1
  | Float _, _ -> t1
  | _, Float _ -> t2
  | _ ->
    let t1' = promote_integer_type conf t1
    and t2' = promote_integer_type conf t2 in
    let size1, signed1 = get_int_type_info t1'
    and size2, signed2 = get_int_type_info t2' in
    if t1' = t2' then
      (* If both operands have the same type, then no further conversion is
         needed. *)
      t1'
    else
      begin
      (* Otherwise, if both operands have signed integer types or both have
         unsigned integer types, the operand with the type of lesser converion
         rank is converted to the type of the operand with greater rank. *)
        if signed1 = signed2 then
          if size2 > size1 then t2' else t1'
        else
          let realsize1 = get_real_int_size conf size1
          and realsize2 = get_real_int_size conf size2 in
          if signed1 then begin
      (* Otherwise, if the operand that has unsigned integer type has rank
         greater than or equal to the rank of the type of the other operand,
         then the operand with signed integer type is converted to the type of
         the operand with unsigned integer type. *)
            if size1 <= size2 then t2'
      (* Otherwise, if the type of the operand with signed integer type can
         represent all of the values of the type of the operand with unsigned
         integer type, then the operand with unsigned integer type is
         converted to the type of the operand with signed integer type. *)
            else if realsize1 > realsize2 then t1'
      (* Otherwise, both operands are converted to the unsigned integer type
         corresponding to the type of the operand with signed integer type. *)
            else (* size1 > size2, realsize1 = realsize2 *)
              Int (size1, false)
          end else begin
            if size2 <= size1 then t1'
            else if realsize2 > realsize1 then t2'
            else (* size1 > size2, realsize1 = realsize2 *)
              Int (size2, false)
          end
      end

(*
let usual_arith_conv' conf e1 e2 =
  match e1.e_type_opt, e2.e_type_opt with
  | Some t1, Some t2 ->
    let result_type = usual_arith_conv conf t1 t2 in
    (mk_conv_if_needed e1 result_type,
     mk_conv_if_needed e2 result_type,
     Some result_type)
  | _ ->
    (e1, e2, None)
*)

let usual_arith_conv' conf op t1 t2 =
  match usual_arith_conv conf t1 t2 with
  | Int (isize, signed) -> lower_binop_i (isize, signed) op
  | Float fsize -> lower_binop_f fsize op
  | _ -> assert false

let type_binary_expr op e1 e2 p : binary_op' option =
  let conf = p.config in
  with_types_of e1.nodeval e2.nodeval begin fun t1 t2 ->
    match op with
    | Mul | Div ->
      ensure_arith_type t1 p (node_loc e1);
      ensure_arith_type t2 p (node_loc e2);
      usual_arith_conv' conf op t1 t2
    | Mod | And | Xor | Or ->
      ensure_arith_type t1 p (node_loc e1);
      ensure_arith_type t2 p (node_loc e2);
      usual_arith_conv' conf op t1 t2
    | Add ->
      if is_arith_type t1 && is_arith_type t2 then begin
        (* num + num *)
        usual_arith_conv' conf op t1 t2
      end else begin
        match t1, t2 with
        | Ptr (t1', cv), (Int _ | Bool) ->
          (* ptr + int *)
          ensure_complete_type t1' p (node_loc e1);
          AddPI (t1', cv)
        | (Int _ | Bool), Ptr (t2', cv) ->
          (* int + ptr *)
          ensure_complete_type t2' p (node_loc e2);
          AddIP (t2', cv)
        | _ ->
          raise Type_Error
      end
    | Sub ->
      if is_arith_type t1 && is_arith_type t2 then
        (* num - num *)
        usual_arith_conv' conf op t1 t2
      else begin
        match t1, t2 with
        | Ptr (t1', cv), (Int _ | Bool) ->
          (* ptr - int *)
          ensure_complete_type t1' p (node_loc e1);
          SubPI (t1', cv)
        | Ptr (t1', cv1), Ptr (t2', cv2) ->
          (* ptr - ptr *)
          ensure_complete_type t1' p (node_loc e1);
          ensure_complete_type t2' p (node_loc e2);
          if t1' <> t2' then begin
            error p (fst (node_loc e1), snd (node_loc e2))
              "incompatible pointer types";
            raise Type_Error
          end;
          SubP (t1', cv1, cv2)
        | _ ->
          error p (fst (node_loc e1), snd (node_loc e2)) "type error";
          raise Type_Error
      end
    | LShift | RShift ->
      ensure_integer_type t1 p (node_loc e1);
      ensure_integer_type t2 p (node_loc e2);
      let t1' = promote_integer_type conf t1 in
      lower_binop_i (get_int_type_info t1') op
    | Lt | Gt | LtEq | GtEq ->
      if is_real_type t1 && is_real_type t2 then begin
        (* num op num *)
        usual_arith_conv' conf op t1 t2
        (* both operands are pointers to compatible object types, or
           compatible incomplete types *)
      end else begin
        match t1, t2 with
        | Ptr (t1', cv1), Ptr (t2', cv2) ->
          (* ptr op ptr *)
          if t1' <> t2' then
            error p (fst (node_loc e1), snd (node_loc e2))
              "incompatible pointer types"
          else if is_func_type t1' then
            warn p (fst (node_loc e1), snd (node_loc e2))
              "ordered comparison on pointers to functions";
          lower_binop_p (t1', cv1, cv2) op
        | _ ->
          error p (fst (node_loc e1), snd (node_loc e2)) "type error";
          raise Type_Error
      end
    | Eq | NotEq ->
      if is_arith_type t1 && is_arith_type t2 then begin
        (* num op num *)
        usual_arith_conv' conf op t1 t2
        (* 1. both are pointers to compatible types
           2. one points to object or incomplete (i.e. non-function) type,
              and the other points to void
           3. one is a pointer, and the other is null pointer constant *)
      end else begin
        let ok =
          match t1, t2 with
          | Ptr (t1', _), Ptr (t2', _) ->
            (* ptr op ptr *)
            t1' = t2' ||
            (not (is_func_type t1') && t2' = Void ||
             not (is_func_type t2') && t1' = Void)
          | Ptr _, _ ->
            (* ptr op NULL *)
            is_nullptr e2.nodeval
          | _, Ptr _ ->
            (* NULL op ptr *)
            is_nullptr e1.nodeval
          | _ -> false
        in
        if not ok then begin
          error p (fst (node_loc e1), snd (node_loc e2)) "type error";
          raise Type_Error
        end;
        lower_binop_i' conf.word_size op
      end
    | LogAnd | LogOr ->
      ensure_scalar_type t1 p (node_loc e1);
      ensure_scalar_type t2 p (node_loc e2);
      begin match op with
        | LogAnd -> LogAnd'
        | LogOr  -> LogOr'
        | _ -> assert false
      end
  end

(* 6.5.2.1 *)
let make_index_expr e1 e2 tokseq p =
  let type_cv_opt =
    with_types_of e1.nodeval e2.nodeval begin fun t1 t2 ->
      match t1, t2 with
      | Ptr (typ, cv), Int _ | Int _, Ptr (typ, cv) -> (typ, cv)
      | _ ->
        (* TODO improve error message *)
        error p (token_seq_loc tokseq) "type error in array subscript";
        raise Type_Error
    end
  in
  let e_type_opt, cv =
    match type_cv_opt with
    | Some (typ, cv) -> Some typ, cv
    | None -> None, no_cv
  in
(*let e1' = promote_to_word_size p.config e1
  and e2' = promote_to_word_size p.config e2 in*)
  mk_node
  { e_kind = E_Index (e1, e2, cv);
    e_type_opt;
    e_value = None } tokseq

(* TODO: argument conversion *)
let make_call_expr e1 es tokseq p =
  let e_type_opt =
    with_type_of e1.nodeval begin fun t1 ->
      match t1 with
      | Ptr (Func { return_type; param_types_opt }, _) ->
        (* check argument types *)
        begin match param_types_opt with
          | Some (param_types, va) ->
            let len_ord = List.compare_lengths param_types es in
            if va then begin
              (* TODO *) ()
            end else begin
              if len_ord = 0 then begin
                List.combine param_types es |>
                List.iter begin fun (arg_type, arg) ->
                  if not (can_assign_to arg.nodeval arg_type) then
                    error p (node_loc arg) "wrong argument type"
                end
              end else error p (token_seq_loc tokseq) "wrong number of arguments"
            end
          | None -> ()
        end;
        return_type
      | _ ->
        error p (node_loc e1) "called object is not a function";
        raise Type_Error
    end
  in
  mk_node
  { e_kind = E_Call (e1, es);
    e_type_opt;
    e_value = None } tokseq

let make_dot_expr e1 field_name tokseq p =
  let base_cv = get_cv e1.nodeval in
  let field_info_opt =
    with_type_of e1.nodeval begin function
      | Struct _ | Union _ as su_type ->
        get_field_info su_type field_name tokseq p
      | _ ->
        error p (node_loc e1) "left hand side of ‘.’ not a struct or union";
        raise Type_Error
    end
  in
  match field_info_opt with
  | Some (field_id, field_type, field_cv) ->
    mk_node
    { e_kind = E_Dot (e1, field_name, Some field_id, cv_max field_cv base_cv);
      e_type_opt = Some field_type;
      e_value = None } tokseq
  | None ->
    mk_node
    { e_kind = E_Dot (e1, field_name, None, base_cv);
      e_type_opt = None;
      e_value = None } tokseq

let make_arrow_expr e1 field_name tokseq p =
  let base_cv =
    Option.default no_cv begin
      with_type_of e1.nodeval begin function
        | Ptr (_, cv) -> cv
        | _ -> raise Type_Error
      end
    end
  in
  let field_info_opt =
    with_type_of e1.nodeval begin function
      | Ptr ((Struct _ | Union _ as su_type), _) ->
        get_field_info su_type field_name tokseq p
      | _ ->
        error p (node_loc e1)
          "left hand side of ‘->’ not a pointer to struct or union";
        raise Type_Error
    end
  in
  match field_info_opt with
  | Some (field_id, field_type, field_cv) ->
    mk_node
    { e_kind = E_Arrow (e1, field_name, Some field_id, cv_max field_cv base_cv);
      e_type_opt = Some field_type;
      e_value = None } tokseq
  | None ->
    mk_node
    { e_kind = E_Arrow (e1, field_name, None, base_cv);
      e_type_opt = None;
      e_value = None } tokseq

let make_inc_dec_expr op e1 p =
  check_inc_dec_operand op e1 p;
  { e_kind = E_IncDec (op, e1);
    e_type_opt = e1.nodeval.e_type_opt;
    e_value = None }

let make_unary_expr op e1 p =
  let low_op_opt = type_unary_expr op e1 p in
  let e_type_opt = Option.map result_type_of_unop low_op_opt in
  { e_kind = E_Unary (op, low_op_opt, e1);
    e_type_opt;
    e_value = eval_unary_expr op e1.nodeval.e_value }

let make_addr_expr e1 p =
  let cv =
    match lvalue_status e1.nodeval.e_kind with
    | Some cv -> cv
    | None ->
      error p (node_loc e1) "operand of ‘&’ is not an lvalue";
      no_cv
  in
  { e_kind = E_Addr e1;
    e_type_opt = Option.map (fun t1 -> Ptr (t1, cv)) e1.nodeval.e_type_opt;
    e_value = None }

let make_deref_expr e1 p =
  let type_cv_opt =
    with_type_of e1.nodeval begin function
      | Ptr (typ, cv) -> typ, cv
      | _ ->
        error p (node_loc e1) "operand of ‘*’ is not of pointer type";
        raise Type_Error
    end
  in
  let e_type_opt, cv =
    match type_cv_opt with
    | Some (typ, cv) -> Some typ, cv
    | None -> None, no_cv
  in
  { e_kind = E_Deref (e1, cv);
    e_type_opt;
    e_value = None }

let type_cond_expr e0 e1 e2 p =
  Option.may (fun t0 -> ensure_scalar_type t0 p (node_loc e0)) (e0.nodeval.e_type_opt);
  with_types_of e1.nodeval e2.nodeval begin fun t1 t2 ->
    begin match t1, t2 with
      | Void, Void -> Void
      | Struct (id1, _), Struct (id2, _) when id1 = id2 -> t1
      | Union (id1, _), Union (id2, _) when id1 = id2 -> t1
      | Ptr (t1', cv1), Ptr (t2', cv2) when t1' = t2' ->
        Ptr (t1', cv_max cv1 cv2)
      | Ptr _, _ when is_nullptr e2.nodeval -> t1
      | _, Ptr _ when is_nullptr e1.nodeval -> t2
      | Ptr (Void, cv1), Ptr (_, cv2)
      | Ptr (_, cv1), Ptr (Void, cv2) ->
        Ptr (Void, cv_max cv1 cv2)
      | _ ->
        if is_arith_type t1 && is_arith_type t2 then
          usual_arith_conv p.config t1 t2
        else begin
          error p (fst (node_loc e1), snd (node_loc e2))
            "incompatible types in conditional expression";
          raise Type_Error
        end
    end
  end

let eval_cond_expr v1_opt v2_opt v3_opt =
  match v1_opt, v2_opt, v3_opt with
  | Some a, Some b, Some c ->
    Some (if a<>0L then b else c)
  | _ -> None

let can_represent_range conf lo hi = function
  | Int (size, signed) ->
    let realsize = get_real_int_size conf size in
    if signed then
      let type_min = Int64.shift_left (-1L) (realsize*8-1) in
      let type_max = Int64.lognot type_min in
      type_min <= lo && hi <= type_max
    else failwith "can_represent_range: cannot handle unsigned types"
  | _ -> assert false

(* for now, only one of the signed integer types will be chosen *)
let choose_enum_type conf lo hi =
  try
    List.find (can_represent_range conf lo hi) [char; short; int]
  with Not_found -> int

let mkbin (op, op_ts) e1 e2 p =
  let low_op_opt = type_binary_expr op e1 e2 p
  and e_loc = fst (node_loc e1), snd (node_loc e2) in
  let e_value =
    try
      eval_binary_expr op e1.nodeval.e_value e2.nodeval.e_value
    with Division_by_zero ->
      warn p e_loc "division by zero in constant expression";
      None
  in
  let e_type_opt = Option.map (result_type_of_binop p.config) low_op_opt in
  mk_node
  { e_kind = E_Binary (op, low_op_opt, e1, e2);
    e_type_opt;
    e_value } (concat_token_seq [e1.tokens; op_ts; e2.tokens])

let mkassign e1 e2 p =
  let e_loc = fst (node_loc e1), snd (node_loc e2) in
  let e_type_opt =
    with_types_of e1.nodeval e2.nodeval begin fun t1 t2 ->
      if not (can_assign_to e2.nodeval t1) then begin
        error p e_loc "type error in assignment";
        raise Type_Error
      end;
      t1
    end
  in
  { e_kind = E_Assign (e1, e2);
    e_type_opt;
    e_value = None }

let new_spec () =
  { base = Unspecified;
    storage = None;
    size = 0;
    sign = None;
    const = false;
    volatile = false }

let starts_type env = function
  | VOID | CHAR | SHORT | INT | LONG | FLOAT | DOUBLE | SIGNED | UNSIGNED
  | BOOL | STRUCT | UNION | ENUM | CONST | VOLATILE ->
    true
  | Ident name ->
    is_typedef_name env name
  | _ ->
    false

let binop_of_token = function
  | Star ->
    Some (Mul, 0)
  | Slash ->
    Some (Div, 0)
  | Percent ->
    Some (Mod, 0)
  | TPlus ->
    Some (Add, 1)
  | TMinus ->
    Some (Sub, 1)
  | LtLt ->
    Some (LShift, 2)
  | GtGt ->
    Some (RShift, 2)
  | Token.Lt ->
    Some (Lt, 3)
  | Token.Gt ->
    Some (Gt, 3)
  | Token.LtEq ->
    Some (LtEq, 3)
  | Token.GtEq ->
    Some (GtEq, 3)
  | EqEq ->
    Some (Eq, 4)
  | BangEq ->
    Some (NotEq, 4)
  | TAnd ->
    Some (And, 5)
  | Circ ->
    Some (Xor, 6)
  | Pipe ->
    Some (Or, 7)
  | AndAnd ->
    Some (LogAnd, 8)
  | PipePipe ->
    Some (LogOr, 9)
  | _ ->
    None

let assign_op_of_token = function
  | Token.Eq ->
    Some Assign
  | StarEq ->
    Some MulAssign
  | SlashEq ->
    Some DivAssign
  | PercentEq ->
    Some ModAssign
  | PlusEq ->
    Some AddAssign
  | MinusEq ->
    Some SubAssign
  | LtLtEq ->
    Some LShiftAssign
  | GtGtEq ->
    Some RShiftAssign
  | AndEq ->
    Some AndAssign
  | CircEq ->
    Some XorAssign
  | PipeEq ->
    Some OrAssign
  | _ ->
    None

let start_pos (_, (startp, _endp)) = startp
let end_pos (_, (_startp, endp)) = endp

(* 6.5.1 *)
let rec parse_primary_expr p =
  start_node p;
  match curtok p with
  | Ident name ->
    skip p;
    let tokseq = end_node p in
    begin
      try
        let v = lookup_obj p.env name in
        let e_type_opt = Some (adjust_array_func_type v.typ) in
        mk_node
        { e_kind = E_Ident (name, Some v);
          e_type_opt;
          e_value = v.value } tokseq
      with Not_found ->
        error p (token_seq_loc tokseq) ("undeclared identifier ‘"^name^"’");
        mk_node
        { e_kind = E_Ident (name, None);
          e_type_opt = None;
          e_value =  None } tokseq
    end
  | TInt (s, signed, size) ->
    skip p;
    mk_node
    { e_kind = E_Lit (IntLit s);
      e_type_opt = Some (Int (size, signed));
      e_value = Some (Preproc.parse_int s) } (end_node p)
  | TFloat (s, size) ->
    skip p;
    mk_node
    { e_kind = E_Lit (FloatLit s);
      e_type_opt = Some (Float size);
      e_value = None } (end_node p)
  | TChar s ->
    skip p;
    mk_node
    { e_kind = E_Lit (CharLit s);
      e_type_opt = Some char;
      e_value = Some (Int64.of_int (Preproc.parse_char s)) } (end_node p)
  | TString s ->
    skip p;
    mk_node
    { e_kind = E_Lit (StringLit s);
      e_type_opt = Some (Ptr (char, mk_cv true false)); (* adjusted *)
      e_value = None } (end_node p)
  | LParen ->
    skip p;
    let e = parse_expr p in
    expect p RParen;
    { e with tokens = end_node p }
  | _ ->
    syntax_error p.tok

(* 6.5.2 *)
and parse_postfix_expr p =
  let rec loop e1 =
    match curtok p with
    | LBrack -> (* a[b] *)
      start_node p;
      skip p;
      let e2 = parse_expr p in
      expect p RBrack;
      let tokseq = join_token_seq e1.tokens (end_node p) in
      loop (make_index_expr e1 e2 tokseq p)
    | LParen -> (* f(...) *)
      start_node p;
      skip p;
      let es = parse_arg_list p in
      expect p RParen;
      let tokseq = join_token_seq e1.tokens (end_node p) in
      loop (make_call_expr e1 es tokseq p)
    | Dot -> (* a.b *)
      start_node p;
      skip p;
      let field_name = expect_ident p in
      let tokseq = join_token_seq e1.tokens (end_node p) in
      loop (make_dot_expr e1 field_name tokseq p)
    | Arrow -> (* a->b *)
      start_node p;
      skip p;
      let field_name = expect_ident p in
      let tokseq = join_token_seq e1.tokens (end_node p) in
      loop (make_arrow_expr e1 field_name tokseq p)
    | PlusPlus -> (* a++ *)
      let tokseq = join_token_seq e1.tokens (skip' p) in
      loop (mk_node (make_inc_dec_expr PostInc e1 p) tokseq)
    | MinusMinus -> (* a-- *)
      let tokseq = join_token_seq e1.tokens (skip' p) in
      loop (mk_node (make_inc_dec_expr PostDec e1 p) tokseq)
    | _ -> e1
  in
  loop (parse_primary_expr p)

(* 6.5.3 *)
and parse_unary_expr p =
  match curtok p with
  | PlusPlus -> (* ++a *)
    start_node p;
    skip p;
    let e1 = parse_unary_expr p in
    let tokseq = end_node p in
    mk_node (make_inc_dec_expr PreInc e1 p) tokseq
  | MinusMinus -> (* --a *)
    start_node p;
    skip p;
    let e1 = parse_unary_expr p in
    let tokseq = end_node p in
    mk_node (make_inc_dec_expr PreDec e1 p) tokseq
  | TPlus | TMinus | Tilde | Bang ->
    let op =
      match curtok p with
      | TPlus -> Plus
      | TMinus -> Minus
      | Tilde -> Not
      | Bang -> LogNot
      | _ -> assert false
    in
    start_node p;
    skip p;
    let e1 = parse_cast_expr p in
    let tokseq = end_node p in
    mk_node (make_unary_expr op e1 p) tokseq
  | TAnd ->
    start_node p;
    skip p;
    let e1 = parse_cast_expr p |> unadjust' in
    let tokseq = end_node p in
    mk_node (make_addr_expr e1 p) tokseq
  | Star ->
    start_node p;
    skip p;
    let e1 = parse_cast_expr p in
    let tokseq = end_node p in
    mk_node (make_deref_expr e1 p) tokseq
  | SIZEOF ->
    let size_of_type' typ tokseq =
      if kind_of_type p.env typ = Object_Type then
        Some (Int64.of_int (size_of_type p.config p.env typ))
      else begin
        error p (token_seq_loc tokseq) "size of operand of ‘sizeof’ is unknown";
        None
      end
    in
    let make_sizeof_e e =
      let e_value =
        match e.nodeval.e_type_opt with
        | Some t -> size_of_type' t e.tokens
        | None -> None
      in
      { e_kind = E_SizeOfE e;
        e_type_opt = Some (size_t p.config);
        e_value }
    and make_sizeof_t tokseq typ =
      { e_kind = E_SizeOfT typ;
        e_type_opt = Some (size_t p.config);
        e_value = size_of_type' typ tokseq }
    in
    start_node p; (* sizeof ... *)
    skip p;
    if curtok p = LParen then begin
      skip p;
      if starts_type p.env (curtok p) then begin
        start_node p;
        let typ = parse_type_name p in
        let tokseq = end_node p in
        expect p RParen;
        mk_node (make_sizeof_t tokseq typ) (end_node p)
      end else begin
        (* sizeof(expr) *)
        let e1 = parse_expr p |> unadjust' in
        expect p RParen;
        mk_node (make_sizeof_e e1) (end_node p)
      end
    end else begin
      (* sizeof expr *)
      let e1 = parse_unary_expr p |> unadjust' in
      mk_node (make_sizeof_e e1) (end_node p)
    end
  | _ ->
    parse_postfix_expr p

(* 6.5.4 *)
and parse_cast_expr p =
  match curtok p with
  | LParen ->
    start_node p;
    skip p;
    if starts_type p.env (curtok p) then begin
      let typ =
        start_node p;
        let typ = parse_type_name p in
        let tokseq = end_node p in
        if not (is_scalar_type typ || typ = Void) then
          error p (token_seq_loc tokseq) "scalar type or void expected";
        typ
      in
      expect p RParen;
      let e = parse_cast_expr p in
      mk_node
      { e_kind = E_Cast (typ, e);
        e_type_opt = Some typ;
        e_value = e.nodeval.e_value } (end_node p)
    end else begin
      let e = parse_expr p in
      expect p RParen;
      { e with tokens = end_node p }
    end
  | _ -> parse_unary_expr p

and parse_arg_list p =
  if curtok p = RParen then [] else begin
    let e = parse_assign_expr p in
    let rec loop acc =
      if curtok p = Comma then begin
        skip p;
        let e = parse_assign_expr p in
        loop (e::acc)
      end else acc
    in
    List.rev (loop [e])
  end

and parse_binary_expr p =
  let es = Stack.create ()
  and ops = Stack.create () in
  Stack.push (parse_cast_expr p) es;
  let rec reduce prec =
    (* until ops is empty or top of ops has lower precedence than prec *)
    let (op, _) = Stack.pop ops in
    let e2 = Stack.pop es in
    let e1 = Stack.pop es in
    let e = mkbin op e1 e2 p in
    Stack.push e es;
    if not (Stack.is_empty ops || snd (Stack.top ops) > prec) then reduce prec
  in
  let rec loop () =
    match binop_of_token (curtok p) with
    | Some (op, prec) ->
      let op_ts = skip' p in
      if not (Stack.is_empty ops || snd (Stack.top ops) > prec) then
        (* ops not empty, and top of ops has precedence higher than or equal
           to prec *)
        reduce prec;
      Stack.push ((op, op_ts), prec) ops;
      Stack.push (parse_cast_expr p) es;
      loop ()
    | None ->
      (* 10 is lower than all operations, so this means reduce until operator
         stack is empty *)
      if not (Stack.is_empty ops) then reduce 10
  in
  loop ();
  Stack.pop es

and parse_cond_expr p =
  let e1 = parse_binary_expr p in
  if curtok p = Quest then begin
    let ts_quest = skip' p in
    let e2 = parse_expr p in
    let ts_colon = skip' p in
    let e3 = parse_cond_expr p in
    let e_type_opt = type_cond_expr e1 e2 e3 p in
    mk_node
    { e_kind = E_Cond (e1, e2, e3);
      e_type_opt;
      e_value = eval_cond_expr e1.nodeval.e_value e2.nodeval.e_value e3.nodeval.e_value }
    (concat_token_seq [e1.tokens; ts_quest; e2.tokens; ts_colon; e3.tokens])
  end else e1

and parse_assign_expr p =
  let e1 = parse_cond_expr p in
  match assign_op_of_token (curtok p) with
  | Some op ->
    let op_ts = skip' p in
    let e2 = parse_assign_expr p in
    let tokseq = (concat_token_seq [e1.tokens; op_ts; e2.tokens]) in
    (* token seq. of the whole assign expr. is NOT the concatenation of
       its components if it's a compound assignment *)
    if op = Assign then mk_node (mkassign e1 e2 p) tokseq else
      mk_node (mkassign e1 (mkbin (binop_of_assign_op op, op_ts) e1 e2 p) p) tokseq
  | None -> e1

and parse_expr p =
  let rec loop e1 =
    if curtok p = Comma then begin
      let ts_comma = skip' p in
      let e2 = parse_assign_expr p in
      let e =
        mk_node
        { e_kind = E_Seq (e1, e2);
          e_type_opt = e2.nodeval.e_type_opt;
          e_value = None; (* "(a,b)" is not a const. expr. by definition *) }
        (concat_token_seq [e1.tokens; ts_comma; e2.tokens])
      in
      loop e
    end else e1
  in
  loop (parse_assign_expr p)

and parse_constant_expr p =
  let e = parse_cond_expr p in
  if e.nodeval.e_value = None then
    error p (node_loc e) "not a constant expression";
  e

and parse_parameter_type_list p =
  let rec loop (acc, va) =
    if curtok p = Comma then begin
      skip p;
      if curtok p = Ellipsis then (skip p; (acc, true))
      else begin
        let decl = parse_decl p in
        loop (decl::acc, va)
      end
    end else (acc, va)
  in
  let (l, va) = loop ([parse_decl p], false) in
  (List.rev l, va)

and parse_direct_declarator allow_anon p =
  let rec loop d1 =
    match curtok p with
    | LParen ->
      start_node p;
      skip p;
      if starts_type p.env (curtok p) then begin
        push_scope p.env; (* function prototype scope *)
        let (l, va) = parse_parameter_type_list p in
        pop_scope p.env;
        expect p RParen;
        let tokseq = end_node p in
        let l' =
          match l with
          | [{ d_type = Void; _ }] -> [] (* f(void) *)
          | _ -> l
        in
        loop (mk_node (D_Func (d1, l', va)) (join_token_seq d1.tokens tokseq))
      end else begin
        match curtok p with
        | Ident name ->
          (* old-style function declarator *)
          skip p;
          let rec loop' acc =
            if curtok p = Comma then begin
              skip p;
              let name' = expect_ident p in
              loop' (name'::acc)
            end else acc
          in
          let l = loop' [name] |> List.rev in
          expect p RParen;
          let tokseq = end_node p in
          loop (mk_node (D_Old_Func (d1, l)) (join_token_seq d1.tokens tokseq))
        | RParen ->
          skip p;
          let tokseq = end_node p in
          loop (mk_node (D_Old_Func (d1, [])) (join_token_seq d1.tokens tokseq))
        | _ ->
          syntax_error p.tok
      end
    | LBrack ->
      start_node p;
      skip p;
      begin match curtok p with
        | RBrack ->
          skip p;
          let tokseq = end_node p in
          loop (mk_node (D_Array (d1, None)) (join_token_seq d1.tokens tokseq))
        | _ ->
          let e = parse_constant_expr p in
          expect p RBrack;
          let tokseq = end_node p in
          loop (mk_node (D_Array (d1, Some e)) (join_token_seq d1.tokens tokseq))
      end
    | _ -> d1
  in
  let d =
    match curtok p with
    | Ident name ->
      let ts = skip' p in
      mk_node (D_Base (Some name)) ts
    | LParen -> (* parse as function declarator: "(T...)" "()" *)
      start_node p;
      skip p;
      begin match curtok p with
      | Ident name as tok when not (starts_type p.env tok) ->
        let d = parse_declarator allow_anon p in
        expect p RParen;
        let tokseq = end_node p in
        { d with tokens = tokseq }
      | _ ->
        ungetsym p;
        let ts = end_node p in (* ts is empty *)
        if allow_anon then mk_node (D_Base None) ts else
          syntax_error p.tok
      end
    | _ ->
      let pos = p.tok.pos in
      let ts = Empty (pos, pos) in
      if allow_anon then mk_node (D_Base None) ts else
        syntax_error p.tok
  in
  loop d

and parse_declarator allow_anon p : string option declarator node =
  match curtok p with
  | Star ->
    start_node p;
    skip p;
    let rec loop (c, v) =
      (* TODO: report duplicate qualifiers *)
      match curtok p with
      | CONST ->
        skip p;
        loop (true, v)
      | VOLATILE ->
        skip p;
        loop (c, true)
      | RESTRICT ->
        skip p;
        loop (c, v) (* TODO *)
      | _ -> (c, v)
    in
    let cv = loop (false, false) in
    let d = parse_declarator allow_anon p in
    mk_node (D_Ptr (d, cv)) (end_node p)
  | _ ->
    parse_direct_declarator allow_anon p

and parse_named_declarator p =
  parse_declarator false p |> make_named_declarator

and parse_initializer p =
  if curtok p = LBrace then begin
    start_node p;
    skip p;
    let i1 = parse_initializer p in
    let rec loop acc =
      if curtok p = Comma then begin
        skip p;
        if curtok p = RBrace then acc
        else loop (parse_initializer p :: acc)
      end else acc
    in
    let il = List.rev (loop [i1]) in
    expect p RBrace;
    Init_List (mk_node il (end_node p))
  end else Init_Expr (parse_assign_expr p)

(* actually enters the declared symbol nto symbol table *)
and parse_init_decl spec p : init_decl =
  let d = make_decl spec (parse_named_declarator p) p in
  let obj = obj_of_decl (gen_id p.env) d in
  declare_object p.env obj;
  match curtok p with
  | Token.Eq ->
    skip p;
    let init = parse_initializer p in
    (* storage should not be typedef *)
    if d.d_storage = Some Typedef then begin
      let loc =
        match init with Init_Expr {tokens;_} | Init_List {tokens;_} ->
          token_seq_loc tokens
      in error p loc "typedef initialized"
    end;
    (d, Some (obj, init))
  | _ ->
    (d, None)

and try_parse_declarator p =
  match curtok p with
  | Ident _ | Star | LBrack | LParen -> parse_declarator true p
  | _ -> mk_node (D_Base None) (Empty (p.tok.pos, p.tok.pos))

and parse_spec p =
  let errmsg = "invalid specifier" in
  let spec = new_spec () in
  begin match curtok p with
    | TYPEDEF ->
      skip p;
      spec.storage <- Some Typedef
    | EXTERN ->
      skip p;
      spec.storage <- Some Extern
    | STATIC ->
      skip p;
      spec.storage <- Some Static
    | _ -> ()
  end;
  let rec loop () =
    let start_pos = p.tok.pos in
    let loc = token_loc p.tok in
    match curtok p with
    | VOID | BOOL | CHAR | INT | FLOAT | DOUBLE ->
      let base =
        match curtok p with
        | VOID -> Spec_Void
        | BOOL -> Spec_Bool
        | CHAR -> Spec_Char
        | INT -> Spec_Int
        | FLOAT -> Spec_Float
        | DOUBLE -> Spec_Double
        | _ -> assert false
      in
      if spec.base = Unspecified then spec.base <- base else
        error p loc errmsg;
      skip p;
      loop ()
    | SHORT ->
      skip p;
      if spec.size = 0 then spec.size <- -1 else
        error p loc errmsg;
      loop ()
    | LONG ->
      skip p;
      begin match spec.size with
        | 0 -> spec.size <- 1
        | 1 -> spec.size <- 2
        | _ -> error p loc errmsg
      end;
      loop ()
    | SIGNED ->
      skip p;
      if spec.sign = None then spec.sign <- Some true else
        error p loc errmsg;
      loop ()
    | UNSIGNED ->
      skip p;
      if spec.sign = None then spec.sign <- Some false else
        error p loc errmsg;
      loop ()
    | CONST ->
      skip p;
      spec.const <- true;
      loop ()
    | VOLATILE ->
      skip p;
      spec.volatile <- true;
      loop ()
    | STRUCT | UNION ->
      let typ = parse_struct_union_spec p in
      if spec.base = Unspecified then spec.base <- Spec_Type typ else
        error p (start_pos, last_end p) errmsg;
      loop ()
    | ENUM ->
      let enum_pos = p.tok.pos in
      skip p;
      let env = p.env in
      let tag_opt = try_read_ident p in
      (* see if a definition is provided *)
      let typ =
        if curtok p = LBrace then begin
          skip p;
          let el = parse_enumerator_list p in
          expect p RBrace;
          let _, lo, hi =
            match el with
            | [] -> assert false
            | (name1, value_opt1) :: tl ->
              let value1 =
                match value_opt1 with
                | Some { nodeval = { e_value = Some value; _ }; _ } -> value
                | _ -> 0L
              in
              define_constant env name1 int value1;
              List.fold_left begin fun (prev_value, lo, hi) (name, value_opt) ->
                let cur_value =
                  match value_opt with
                  | Some { nodeval = { e_value = Some value; _ }; _ } -> value
                  | _ -> Int64.add prev_value 1L
                in
                define_constant env name int cur_value;
                (cur_value, min cur_value lo, max cur_value hi)
              end (value1, value1, value1) tl
          in
          let enum_type = choose_enum_type p.config lo hi in
          (* TODO: report redifinition *)
          Option.may (fun name -> declare_tag env name (Tag_Enum enum_type))
            tag_opt;
          enum_type
        end else begin
          match tag_opt with
          | Some tag ->
            begin match H.find env.tag_table tag with
              | Tag_Enum typ ->
                typ
              | _ ->
                (* wrong tag *)
                raise Type_Error
              | exception Not_found ->
                (* The tag is not defined, and no definition is provided.
                   This is an error. *)
                error p (enum_pos, last_end p) "enum type is not defined";
                raise Type_Error
            end
          | None ->
            (* No tag, and no definition, just a bare "enum". *)
            error p (enum_pos, last_end p) "invalid ‘enum’";
            raise Type_Error
        end
      in
      if spec.base = Unspecified then spec.base <- Spec_Type typ else
        error p loc errmsg;
      loop ()
    | Ident name when is_typedef_name p.env name ->
      skip p;
      let typ, cv = get_typedef_type p.env name in
      (* TODO: report duplicate qualifiers *)
      if cv_is_const cv then spec.const <- true;
      if cv_is_volatile cv then spec.volatile <- true;
      if spec.base = Unspecified then spec.base <- Spec_Type typ
      else error p loc errmsg;
      loop ()
    | _ -> ()
  in
  loop ();
  spec

and parse_struct_union_spec p =
  let wrong_tag_error name =
    (Printf.sprintf "‘%s’ defined as wrong kind of tag" name)
  in
  let keyword_loc = (p.tok.pos, last_end p) in
  let is_union =
    match getsym p with
    | STRUCT -> false
    | UNION -> true
    | _ -> assert false
  in
  let env = p.env in
  let table = env.su_table in
  let new_id tag_opt =
    let id = DynArray.length table in
    DynArray.add table { name = tag_opt; def = None };
    let tag_type = if is_union then Tag_Union id else Tag_Struct id in
    Option.may (fun tag -> declare_tag env tag tag_type) tag_opt;
    id
  in
  let tag_pos = p.tok.pos in
  let tag_opt = try_read_ident p in
  let id =
    match tag_opt with
    | Some tag ->
      begin match H.find env.tag_table tag with
        | Tag_Struct id ->
          if is_union then
            error p (tag_pos, last_end p) (wrong_tag_error tag);
          id
        | Tag_Union id ->
          if not is_union then
            error p (tag_pos, last_end p) (wrong_tag_error tag);
          id
        | Tag_Enum _ ->
          error p (tag_pos, last_end p) (wrong_tag_error tag);
          new_id tag_opt
        | exception Not_found -> new_id tag_opt
      end
    | None -> new_id None
  in
  if curtok p = LBrace then begin
    skip p;
    let def = parse_struct_decl_list p |> Array.of_list in
    expect p RBrace;
    let name_def = DynArray.get table id in
    (* TODO: report redefinition *)
    name_def.def <- Some def
  end else if tag_opt = None then error p keyword_loc "invalid specifier";
  if is_union then Union (id, tag_opt) else Struct (id, tag_opt)

and parse_enumerator_list p =
  let parse_enumerator p =
    let name = expect_ident p in
    let value_opt =
      if curtok p = Eq then begin
        skip p;
        let e = parse_constant_expr p in
        Some e
      end else None
    in
    (name, value_opt)
  in
  let rec loop acc =
    match curtok p with
    | Comma ->
      skip p;
      if curtok p = RBrace then acc else
        loop (parse_enumerator p :: acc)
    | _ -> acc
  in
  loop [parse_enumerator p] |> List.rev

and parse_struct_decl_list p =
  let rec loop acc =
    if starts_type p.env (curtok p) then begin
      let d = parse_struct_declaration p in
      loop (d @ acc)
    end else acc
  in
  let d = parse_struct_declaration p in
  loop d |> List.rev

and parse_decl p : string option pdecl =
  let spec = parse_spec p in
  let d = try_parse_declarator p in
  make_decl spec d p

(* consumes terminating semicolon *)
and parse_declaration p : init_decl list =
  let spec = parse_spec p in
  if curtok p = Semi then (skip p; [])
  else begin
    let collect acc = parse_init_decl spec p :: acc in
    let rec loop acc =
      match curtok p with
      | Semi -> skip p; acc
      | Comma -> skip p; loop (collect acc)
      | _ -> acc
    in
    loop (collect []) |> List.rev
  end

and parse_struct_declaration p =
  let spec = parse_spec p in
  let cv = make_cv spec
  and typ = make_type spec in
  if curtok p = Semi then begin
    skip p;
    []
  end else begin
    let rec loop acc =
      match curtok p with
      | Semi -> skip p; acc
      | Comma ->
        skip p;
        loop (parse_named_declarator p :: acc)
      | _ -> acc
    in
    loop [parse_named_declarator p] |> List.rev_map begin fun dr ->
      make_decl' spec.storage cv typ dr p
    end
  end

and parse_type_name p =
  let { d_type; _ } = parse_decl p in
  d_type

(* statements *)

let rec parse_stmt p =
  match curtok p with
  | CASE ->
    start_node p;
    skip p;
    let case_expr = parse_constant_expr p in
    expect p Colon;
    let s = parse_stmt p in
    mk_node (S_Label (Case_Label case_expr, s)) (end_node p)
  | DEFAULT ->
    start_node p;
    skip p;
    expect p Colon;
    let s = parse_stmt p in
    mk_node (S_Label (Default_Label, s)) (end_node p)
  | Semi ->
    let ts = skip' p in
    mk_node S_Null ts
  | LBrace ->
    start_node p;
    let b = parse_block p in
    mk_node (S_Comp b) (end_node p)
  | GOTO ->
    start_node p;
    skip p;
    let label = expect_ident p in
    expect p Semi;
    mk_node (S_Goto label) (end_node p)
  | CONTINUE ->
    start_node p;
    skip p;
    expect p Semi;
    mk_node S_Continue (end_node p)
  | BREAK ->
    start_node p;
    skip p;
    expect p Semi;
    mk_node S_Break (end_node p)
  | RETURN ->
    start_node p;
    skip p;
    let nodeval =
      if curtok p = Semi then begin
        skip p;
        S_Return None
      end else begin
        let e = parse_expr p in
        expect p Semi;
        S_Return (Some e)
      end
    in mk_node nodeval (end_node p)
  | IF ->
    start_node p;
    skip p;
    expect p LParen;
    let e = parse_expr p in
    expect p RParen;
    let s1 = parse_stmt p in
    let nodeval =
      match curtok p with
      | ELSE ->
        skip p;
        let s2 = parse_stmt p in
        S_IfElse (e, s1, s2)
      | _ ->
        S_If (e, s1)
    in mk_node nodeval (end_node p)
  | SWITCH ->
    start_node p;
    skip p;
    expect p LParen;
    let e = parse_expr p in
    expect p RParen;
    let s = parse_stmt p in
    mk_node (S_Switch (e, s)) (end_node p)
  | WHILE ->
    start_node p;
    skip p;
    expect p LParen;
    let e = parse_expr p in
    expect p RParen;
    let s = parse_stmt p in
    mk_node (S_While (e, s)) (end_node p)
  | DO ->
    start_node p;
    skip p;
    let s = parse_stmt p in
    expect p WHILE;
    expect p LParen;
    let e = parse_expr p in
    expect p RParen;
    expect p Semi;
    mk_node (S_DoWhile (s, e)) (end_node p)
  | FOR ->
    start_node p;
    skip p;
    expect p LParen;
    let nodeval =
      if starts_type p.env (curtok p) then begin
        let d = parse_declaration p in
        let e2 =
          match curtok p with
          | Semi ->
            skip p;
            None
          | _ ->
            let e = parse_expr p in
            expect p Semi;
            Some e
        in
        let e3 =
          match curtok p with
          | RParen ->
            skip p;
            None
          | _ ->
            let e = parse_expr p in
            expect p RParen;
            Some e
        in
        let s = parse_stmt p in
        S_For2 (d, e2, e3, s)
      end else begin
        let e1 =
          match curtok p with
          | Semi ->
            skip p;
            None
          | _ ->
            let e = parse_expr p in
            expect p Semi;
            Some e
        in
        let e2 =
          match curtok p with
          | Semi ->
            skip p;
            None
          | _ ->
            let e = parse_expr p in
            expect p Semi;
            Some e
        in
        let e3 =
          match curtok p with
          | RParen ->
            skip p;
            None
          | _ ->
            let e = parse_expr p in
            expect p RParen;
            Some e
        in
        let s = parse_stmt p in
        S_For1 (e1, e2, e3, s)
      end
    in mk_node nodeval (end_node p)
  | Ident name ->
    (* Is this a label? *)
    start_node p;
    skip p;
    let nodeval =
      if curtok p = Colon then begin
        skip p;
        S_Label (Ordinary_Label name, parse_stmt p)
      end else begin
        ungetsym p;
        let e = parse_expr p in
        expect p Semi;
        S_Expr e
      end
    in mk_node nodeval (end_node p)
  | _ ->
    start_node p;
    let e = parse_expr p in
    expect p Semi;
    mk_node (S_Expr e) (end_node p)

and parse_block p =
  skip p; (* '{' *)
  push_scope p.env;
  let rec loop acc =
    if curtok p = RBrace then (skip p; acc) else begin
      let item =
        if starts_type p.env (curtok p) then
          Item_Decl (parse_declaration p)
        else
          Item_Stmt (parse_stmt p)
      in
      loop (item::acc)
    end
  in
  let result = loop [] |> List.rev in
  pop_scope p.env;
  result

let parse_extern_decl p =
  let spec = parse_spec p in
  if curtok p = Semi then (skip p; Decl []) else begin
    let (d, init_opt) = parse_init_decl spec p in
    match curtok p with
    | Semi | Comma ->
      let rec loop acc =
        match curtok p with
        | Semi -> skip p; acc
        | Comma ->
          skip p;
          loop (parse_init_decl spec p :: acc)
        | _ ->
          error p (token_loc p.tok) "syntax error";
          acc
      in
      Decl (List.rev (loop [(d, init_opt)]))
    | _ ->
      (* should be function dfinition *)
      let ensure_no_init = function
        | None -> ()
        | Some (_, (Init_Expr { tokens; _ } | Init_List { tokens; _ })) ->
          error p (token_seq_loc tokens) "no initializer allowed"
      in
      (* the init declarator should not contain an initializer *)
      ensure_no_init init_opt;
      (* take care of parameter declarations *)
      push_scope p.env;
      begin match d.d_declarator with
        | { nodeval = D_Func (_, pdecls, _); _ } ->
          pdecls |> List.iter begin fun pd ->
            match make_named_decl_opt pd with
            | None -> ()
            | Some pd' -> declare p.env pd'
          end
        | { nodeval = D_Old_Func (_, pnames); _ } ->
          let rec loop () =
            if curtok p <> LBrace then begin
              let idecls = parse_declaration p in
              idecls |> List.iter begin fun (pd, init_opt) ->
                ensure_no_init init_opt;
                declare p.env pd
              end;
              loop ()
            end
          in loop ()
        | { tokens; _ } ->
          error p (token_seq_loc tokens) "function declarator expected"
      end;
      let block = parse_block p in
      pop_scope p.env;
      let locals = List.rev p.env.local_objects_rev in
      p.env.local_id <- 0;
      p.env.local_objects_rev <- [];
      Func_Def (d, block, locals)
  end

let parse_translation_unit p =
  let rec loop acc =
    if curtok p = EOF then acc else loop (parse_extern_decl p :: acc)
  in
  loop [] |> List.rev
