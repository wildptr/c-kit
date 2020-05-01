(* not-so-abstract syntax tree *)

open ExtLib

type int_size =
  | Size_Char
  | Size_Short
  | Size_Int
  | Size_Long
  | Size_Long_Long
[@@deriving show { with_path = false }]

type float_size =
  | Size_Float
  | Size_Double
  | Size_Long_Double
[@@deriving show { with_path = false }]

type cv = bool * bool

let no_cv = (false, false)

let cv_le (c1,v1) (c2,v2) =
  c1 <= c2 && v1 <= v2

let cv_max (c1,v1) (c2,v2) =
  (max c1 c2, max v1 v2)

let cv_is_const cv = fst cv
let cv_is_volatile cv = snd cv

let mk_cv c v = (c, v)

type storage = Typedef | Extern | Static

type typ =
  | Void
  | Int of int_size * bool
  | Float of float_size
  | Ptr of typ * cv
  | Func of func_type
  | Array of typ * cv * int
  | Incomplete_Array of typ * cv
  | Bool
  | Struct of int
  | Union of int

and func_type = {
  return_type : typ;
  param_types_opt : unit pdecl list option
}

(* "polymorphic" declaration *)
and 'a pdecl = {
  d_storage : storage option;
  d_cv : cv;
  d_type : typ;
  d_name : 'a (* 'a can be one of {unit, string option, string} *)
}

type decl = string pdecl

let make_named_decl_opt { d_storage; d_cv; d_type; d_name } =
  match d_name with
  | None -> None
  | Some name -> Some { d_storage; d_cv; d_type; d_name = name }

let pp_cv f (c,v) =
  let open Format in
  if c then
    pp_print_string f "const";
  if v then
    if c then pp_print_char f ' ';
    pp_print_string f "volatile"

let pp_base_type_cv ~space m f (typ, cv) =
  let open Format in
  if cv_is_const cv then
    pp_print_string f "const ";
  if cv_is_volatile cv then
    pp_print_string f "volatile ";
  begin match typ with
    | Void ->
      pp_print_string f "void"
    | Int (size, signed) ->
      if not signed then pp_print_string f "unsigned ";
      begin match size with
        | Size_Char ->
          pp_print_string f "char";
        | Size_Short ->
          pp_print_string f "short int";
        | Size_Int ->
          pp_print_string f "int";
        | Size_Long ->
          pp_print_string f "long int";
        | Size_Long_Long ->
          pp_print_string f "long long int";
      end
    | Bool ->
      pp_print_string f "_Bool"
    | Float Size_Float ->
      pp_print_string f "float"
    | Float Size_Double ->
      pp_print_string f "double"
    | Float Size_Long_Double ->
      pp_print_string f "long double"
    | Struct id ->
      fprintf f "struct #%d" id
    | Union id ->
      fprintf f "union #%d" id
    | _ -> assert false
  end;
  if space then pp_print_char f ' ';
  m f ()

let rec pp_typ_aux f (typ, cv) ~space ~enclose m =
  let open Format in
  match typ with
  | Ptr (typ', cv') ->
    pp_typ_aux f (typ', cv') ~space:true ~enclose:false
      (fun f () ->
         let opt_space = if cv = no_cv then "" else " " in
         if enclose then
           fprintf f "(*%a%s%a)" pp_cv cv opt_space m ()
         else
           fprintf f "*%a%s%a" pp_cv cv opt_space m ())
  | Array (typ', cv', n) ->
    pp_typ_aux f (typ', cv') ~space:true ~enclose:true
      (fun f () ->
         fprintf f "%a[%d]" m () n)
  | Incomplete_Array (typ', cv') ->
    pp_typ_aux f (typ', cv') ~space:true ~enclose:true
      (fun f () ->
         fprintf f "%a[]" m ())
  | Func { return_type; param_types_opt } ->
    pp_typ_aux f (return_type, no_cv) ~space:true ~enclose:true
      (fun f () ->
         fprintf f "%a(" m ();
         begin match param_types_opt with
           | Some param_types ->
             pp_parameter_type_list f param_types
           | None -> ()
         end;
         pp_print_char f ')')
  | _ ->
    pp_base_type_cv ~space m f (typ, cv)

and pp_parameter_type_list f l =
  let open Format in
  match l with
  | [] -> pp_print_string f "void"
  | hd::tl ->
    pp_typ_cv f (hd.d_type, hd.d_cv);
    tl |> List.iter begin fun d ->
      pp_print_string f ", ";
      pp_typ_cv f (d.d_type, d.d_cv)
    end

and pp_typ_cv f (typ, cv) =
  pp_typ_aux f (typ, cv) ~space:false ~enclose:false (fun f () -> ())

and pp_typ f typ =
  pp_typ_cv f (typ, no_cv)

let is_integer_type = function
  | Int _ | Bool -> true
  | _ -> false

let is_real_type = function
  | Int _ | Bool | Float _ -> true
  | _ -> false

let is_arith_type = is_real_type

let is_scalar_type = function
  | Int _ | Float _ | Ptr _ | Bool -> true
  | _ -> false

let is_func_type = function
  | Func _ -> true
  | _ -> false

let int = Int (Size_Int, true)
let unsigned_int = Int (Size_Int, false)
let char = Int (Size_Char, true)
let short = Int (Size_Short, true)

type lit =
  | IntLit of string
  | FloatLit of string
  | CharLit of string
  | StringLit of string
[@@deriving show { with_path = false }]

type inc_dec_op =
  | PostInc
  | PostDec
  | PreInc
  | PreDec
[@@deriving show { with_path = false }]

type unary_op =
  | Plus
  | Minus
  | Not
  | LogNot
[@@deriving show { with_path = false }]

type unary_op' =
  | PlusI of int_size * bool
  | PlusF of float_size
  | MinusI of int_size * bool
  | MinusF of float_size
  | NotI of int_size * bool
  | LogNot'
[@@deriving show { with_path = false }]

let lower_unop_i (isize, signed) = function
  | Plus  -> PlusI  (isize, signed)
  | Minus -> MinusI (isize, signed)
  | Not   -> NotI   (isize, signed)
  | _ -> assert false

let lower_unop_f fsize = function
  | Plus  -> PlusF  fsize
  | Minus -> MinusF fsize
  | _ -> assert false

let result_type_of_unop = function
  | PlusI  (size, signed)
  | MinusI (size, signed)
  | NotI   (size, signed) -> Int (size, signed)

  | PlusF  size
  | MinusF size -> Float size

  | LogNot' -> int

let inc_dec_op_name = function
  | PostInc | PreInc -> "++"
  | PostDec | PreDec -> "--"

let unary_op_name = function
  | Plus -> "+"
  | Minus -> "-"
  | Not -> "~"
  | LogNot -> "!"

type binary_op =
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | LShift
  | RShift
  | Lt
  | Gt
  | LtEq
  | GtEq
  | Eq
  | NotEq
  | And
  | Xor
  | Or
  | LogAnd
  | LogOr
[@@deriving show { with_path = false }]

type assign_op =
  | Assign
  | MulAssign
  | DivAssign
  | ModAssign
  | AddAssign
  | SubAssign
  | LShiftAssign
  | RShiftAssign
  | AndAssign
  | XorAssign
  | OrAssign
  | Seq
[@@deriving show { with_path = false }]

type binary_op' =
  | MulI of int_size * bool
  | MulF of float_size
  | DivI of int_size * bool
  | DivF of float_size
  | ModI of int_size * bool
  | AddI of int_size * bool
  | AddF of float_size
  | AddPI of typ * cv
  | AddIP of typ * cv
  | SubI of int_size * bool
  | SubF of float_size
  | SubPI of typ * cv
  | SubP of typ * cv * cv
  | LShiftI of int_size * bool
  | RShiftI of int_size * bool
  | LtI of int_size * bool
  | LtP of typ * cv * cv
  | LtF of float_size
  | GtI of int_size * bool
  | GtP of typ * cv * cv
  | GtF of float_size
  | LtEqI of int_size * bool
  | LtEqP of typ * cv * cv
  | LtEqF of float_size
  | GtEqI of int_size * bool
  | GtEqP of typ * cv * cv
  | GtEqF of float_size
  | EqI of int_size
  | EqF of float_size
  | NotEqI of int_size
  | NotEqF of float_size
  | AndI of int_size * bool
  | OrI of int_size * bool
  | XorI of int_size * bool
  | LogAnd'
  | LogOr'
[@@deriving show { with_path = false }]

type config = {
  short_size : int;
  int_size : int;
  long_size : int;
  long_long_size : int;
  word_size : int_size
}

let size_t conf =
  Int (conf.word_size, false)

let ptrdiff_t conf =
  Int (conf.word_size, true)

let result_type_of_binop conf = function
  | MulI    (size, signed)
  | DivI    (size, signed)
  | ModI    (size, signed)
  | AddI    (size, signed)
  | SubI    (size, signed)
  | LShiftI (size, signed)
  | RShiftI (size, signed)
  | AndI    (size, signed)
  | OrI     (size, signed)
  | XorI    (size, signed) -> Int (size, signed)

  | MulF size
  | DivF size
  | AddF size
  | SubF size -> Float size

  | AddPI (typ, cv)
  | AddIP (typ, cv)
  | SubPI (typ, cv) -> Ptr (typ, cv)

  | SubP (typ, _, _) -> ptrdiff_t conf

  | LtI _
  | LtP _
  | LtF _
  | GtI _
  | GtP _
  | GtF _
  | LtEqI _
  | LtEqP _
  | LtEqF _
  | GtEqI _
  | GtEqP _
  | GtEqF _
  | EqI _
  | EqF _
  | NotEqI _
  | NotEqF _
  | LogAnd'
  | LogOr' -> int

let lower_binop_i (isize, signed) = function
  | Mul    -> MulI    (isize, signed)
  | Div    -> DivI    (isize, signed)
  | Mod    -> ModI    (isize, signed)
  | Add    -> AddI    (isize, signed)
  | Sub    -> SubI    (isize, signed)
  | LShift -> LShiftI (isize, signed)
  | RShift -> RShiftI (isize, signed)
  | Lt     -> LtI     (isize, signed)
  | Gt     -> GtI     (isize, signed)
  | LtEq   -> LtEqI   (isize, signed)
  | GtEq   -> GtEqI   (isize, signed)
  | And    -> AndI    (isize, signed)
  | Xor    -> XorI    (isize, signed)
  | Or     -> OrI     (isize, signed)
  | _ -> assert false

let lower_binop_i' isize = function
  | Eq    -> EqI    isize
  | NotEq -> NotEqI isize
  | _ -> assert false

let lower_binop_p (typ, cv1, cv2) = function
  | Lt   -> LtP   (typ, cv1, cv2)
  | Gt   -> GtP   (typ, cv1, cv2)
  | LtEq -> GtEqP (typ, cv1, cv2)
  | GtEq -> GtEqP (typ, cv1, cv2)
  | _ -> assert false

let lower_binop_f fsize = function
  | Mul    -> MulF    fsize
  | Div    -> DivF    fsize
  | Add    -> AddF    fsize
  | Sub    -> SubF    fsize
  | Lt     -> LtF     fsize
  | Gt     -> GtF     fsize
  | LtEq   -> LtEqF   fsize
  | GtEq   -> GtEqF   fsize
  | Eq     -> EqF     fsize
  | NotEq  -> NotEqF  fsize
  | _ -> assert false

let binop_of_assign_op = function
  | MulAssign    -> Mul
  | DivAssign    -> Div
  | ModAssign    -> Mod
  | AddAssign    -> Add
  | SubAssign    -> Sub
  | LShiftAssign -> LShift
  | RShiftAssign -> RShift
  | AndAssign    -> And
  | XorAssign    -> Xor
  | OrAssign     -> Or
  | _ -> assert false

type obj = {
  name : string;
  typ : typ;
  storage : storage option;
  value : int64 option;
  cv : cv
}

let obj_of_decl d =
  { name = d.d_name;
    typ = d.d_type;
    storage = d.d_storage;
    value = None;
    cv = d.d_cv }

type loc = Lexing.position * Lexing.position

let pp_loc f ((pos0 : Lexing.position), (pos1: Lexing.position)) =
  let file = pos0.pos_fname
  and line_start = pos0.pos_lnum
  and col_start = pos0.pos_cnum - pos0.pos_bol
  and line_end = pos1.pos_lnum
  and col_end = pos1.pos_cnum - pos1.pos_bol in
  Format.fprintf f "%s:%d:%d-%d:%d" file line_start col_start line_end col_end

let pp_pdecl pp_name f d =
  let open Format in
  begin match d.d_storage with
    | Some Typedef ->
      pp_print_string f "typedef "
    | Some Extern ->
      pp_print_string f "extern "
    | Some Static ->
      pp_print_string f "static "
    | None -> ()
  end;
  pp_typ_aux f (d.d_type, d.d_cv) ~space:true ~enclose:false
(*     (fun f () -> Option.may (pp_print_string f) d.d_name) *)
    (fun f () -> pp_name f d.d_name)

let pp_decl f d = pp_pdecl Format.pp_print_string f d

let pp_obj f v =
  let open Format in
  let d =
    { d_storage = v.storage;
      d_cv = v.cv;
      d_type = v.typ;
      d_name = v.name }
  in
  pp_decl f d;
  match v.value with
  | Some x -> Format.fprintf f " = %Ld" x
  | None -> ()

type expr_kind =
  | E_Ident of string * obj option
  | E_Lit of lit
  | E_Call of expr * expr list
  | E_Unary of unary_op * unary_op' option * expr
  | E_IncDec of inc_dec_op * expr
  | E_Addr of expr
  | E_Binary of binary_op * binary_op' option * expr * expr
  | E_Assign of expr * expr
  | E_Seq of expr * expr
  | E_Cond of expr * expr * expr
  | E_SizeOfE of expr
  | E_SizeOfT of typ
  | E_Cast of typ * expr
  | E_Deref of expr * cv
  | E_Dot of expr * string * int option * cv
  | E_Arrow of expr * string * int option * cv
  | E_Index of expr * expr * cv
  | E_Conv of expr

and expr = {
  e_kind : expr_kind;
  e_type_opt : typ option;
  e_value : int64 option;
  e_loc : loc
}

[@@deriving show { with_path = false }]

let has_type e =
  e.e_type_opt <> None

let get_type e =
  Option.get e.e_type_opt

let is_nullptr e =
  has_type e && is_integer_type (get_type e) && e.e_value = Some 0L

type init =
  | Init_Expr of expr
  | Init_List of init list * loc

type init_decl = decl * init option

let rec pp_init_list f il =
  let open Format in
  match il with
  | Init_Expr e ->
    pp_expr f e
  | Init_List (l, _loc) ->
    pp_print_char f '{';
    begin match l with
      | [] -> ()
      | hd::tl ->
        fprintf f " %a" pp_init_list hd;
        List.iter (fprintf f ", %a" pp_init_list) tl
    end;
    pp_print_string f " }"

let pp_init_decl f (d, init) =
  pp_decl f d;
  match init with
  | None -> ()
  | Some il ->
    Format.fprintf f " = %a" pp_init_list il

type struct_union_def = decl array

(* type init_decl = decl * expr *)

type label =
  | Ordinary_Label of string
  | Case_Label of expr
  | Default_Label
[@@deriving show { with_path = false }]

type stmt =
  | S_Null
  | S_Label of label * stmt
  | S_Comp of block
  | S_Expr of expr
  | S_If of expr * stmt
  | S_IfElse of expr * stmt * stmt
  | S_Switch of expr * stmt
  | S_While of expr * stmt
  | S_DoWhile of stmt * expr
  | S_For1 of expr option * expr option * expr option * stmt
  | S_For2 of init_decl list * expr option * expr option * stmt
  | S_Goto of string
  | S_Continue
  | S_Break
  | S_Return of expr option

and block_item =
  | Item_Decl of init_decl list
  | Item_Stmt of stmt

and block = block_item list

[@@deriving show { with_path = false }]

let pp_func_type f ft =
  pp_typ f (Func ft)

type extern_decl =
  | Func_Def of decl * block
  | Decl of init_decl list
[@@deriving show { with_path = false }]
