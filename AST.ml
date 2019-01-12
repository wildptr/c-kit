type lit =
  | IntLit of string
  | FloatLit of string
  | CharLit of string
  | StringLit of string
[@@deriving show]

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
[@@deriving show]

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
[@@deriving show]

type type_spec =
  | Void
  | Char
  | Bool
  | Short
  | Int
  | Long
  | Float
  | Double
  | Signed
  | Unsigned
[@@deriving show]

type type_qual = Const | Volatile [@@deriving show]

type type_spec_qual =
  | TypeSpec of type_spec
  | TypeQual of type_qual
[@@deriving show]

type sc_spec =
  | Typedef
  | Extern
  | Static
[@@deriving show]

type decl_spec =
  | DeclStorageClass of sc_spec
  | DeclTypeSpec of type_spec
  | DeclTypeQual of type_qual
[@@deriving show]

type expr =
  | IdentExpr of string
  | LitExpr of lit
  | CallExpr of expr * expr list
  | UnaryExpr of unary_op * expr
  | BinaryExpr of binary_op * expr * expr
  | CondExpr of expr * expr * expr
  | SizeOfExpr of typ
  | CastExpr of typ * expr

and declarator =
  | IdentDeclarator of string
  | ArrayDeclarator of declarator * expr
  | FuncDeclarator of declarator * parameter_decl list
  | OldFuncDeclarator of declarator * string list
  | PtrDeclarator of type_qual list * declarator

and abstract_declarator =
  | NullAbsDeclarator
  | ArrayAbsDeclarator of abstract_declarator * expr
  | FuncAbsDeclarator of abstract_declarator * parameter_decl list
  | PtrAbsDeclarator of type_qual list * abstract_declarator

and parameter_decl =
  | NamedParamDecl of decl_spec list * declarator
  | AbsParamDecl of decl_spec list * abstract_declarator

and typ = type_spec_qual list * abstract_declarator

[@@deriving show]

type init_declarator = declarator * expr option
[@@deriving show]

type decl = decl_spec list * init_declarator list
[@@deriving show]

type stmt =
  | NullStmt
  | LabelStmt of string * stmt
  | CaseLabelStmt of expr * stmt
  | DefaultLabelStmt of stmt
  | CompStmt of block_item list
  | ExprStmt of expr
  | IfStmt of expr * stmt * stmt
  | SwitchStmt of expr * stmt
  | WhileStmt of expr * stmt
  | DoWhileStmt of stmt * expr
  | ForStmt1 of expr option * expr option * expr option * stmt
  | ForStmt2 of decl * expr option * expr option * stmt
  | GotoStmt of string
  | ContinueStmt
  | BreakStmt
  | ReturnStmt of expr option

and block_item =
  | DeclItem of decl
  | StmtItem of stmt

[@@deriving show]

type func_def = decl_spec list * declarator * decl list * block_item list
[@@deriving show]

type extern_decl =
  | FuncDef of func_def
  | Decl of decl
[@@deriving show]
