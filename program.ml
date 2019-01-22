type int_size =
  | Size_Char
  | Size_Short
  | Size_Int
  | Size_Long
  | Size_Long_Long
[@@deriving show { with_path = false }]

type cv = bool
[@@deriving show { with_path = false }]

type typ =
  | Void
  | Int of int_size * bool
  | Ptr of typ * cv
  | Func of func_type

and func_type = {
  return_type : typ;
  param_types : typ list
}

[@@deriving show { with_path = false }]

type storage = Typedef | Extern | Static
[@@deriving show { with_path = false }]

type lit =
  | IntLit of string
  | FloatLit of string
  | CharLit of string
  | StringLit of string
[@@deriving show { with_path = false }]

type unary_op =
  | Dot of string
  | Arrow of string
  | PostInc
  | PostDec
  | PreInc
  | PreDec
  | Addr
  | Deref
  | Plus
  | Minus
  | Not
  | LogNot
  | SizeOf
[@@deriving show { with_path = false }]

type binary_op =
  | Index
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

type var = {
  name : string;
  typ : typ;
  storage : storage option
}
[@@deriving show { with_path = false }]

type expr =
  | VarExpr of var
  | LitExpr of lit
  | CallExpr of expr * expr list
  | UnaryExpr of unary_op * expr
  | BinaryExpr of binary_op * expr * expr
  | CondExpr of expr * expr * expr
  | SizeOfExpr of typ
  | CastExpr of typ * expr
[@@deriving show { with_path = false }]

type decl = var
[@@deriving show { with_path = false }]

(* type init_decl = decl * expr *)

type label =
  | OrdinaryLabel of string
  | CaseLabel of expr
  | DefaultLabel
[@@deriving show { with_path = false }]

type stmt =
  | NullStmt
  | LabelStmt of label * stmt
  | CompStmt of block
  | ExprStmt of expr
  | IfStmt of expr * stmt
  | IfElseStmt of expr * stmt * stmt
  | SwitchStmt of expr * stmt
  | WhileStmt of expr * stmt
  | DoWhileStmt of stmt * expr
  | ForStmt of expr option * expr option * expr option * stmt
  | GotoStmt of string
  | ContinueStmt
  | BreakStmt
  | ReturnStmt of expr option

and block = {
  decl : decl list;
  body : stmt list
}

[@@deriving show { with_path = false }]

type func_def = {
  name : string;
  typ : func_type;
  block : block
}
[@@deriving show { with_path = false }]

type extern_decl =
  | FuncDef of func_def
  | Decl of decl
[@@deriving show { with_path = false }]
