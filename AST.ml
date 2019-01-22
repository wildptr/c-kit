open Program

type spec_qual =
  | S_Void
  | S_Char
  | S_Bool
  | S_Short
  | S_Int
  | S_Long
  | S_Float
  | S_Double
  | S_Signed
  | S_Unsigned
  | S_Const
  | S_Volatile
  | S_Typedef
  | S_Extern
  | S_Static
[@@deriving show { with_path = false }]

type s_expr =
  | S_IdentExpr of string
  | S_LitExpr of lit
  | S_CallExpr of s_expr * s_expr list
  | S_UnaryExpr of unary_op * s_expr
  | S_BinaryExpr of binary_op * s_expr * s_expr
  | S_CondExpr of s_expr * s_expr * s_expr
  | S_SizeOfExpr of s_typ
  | S_CastExpr of s_typ * s_expr

and declarator =
  | IdentDeclarator of string
  | ArrayDeclarator of declarator * s_expr
  | FuncDeclarator of declarator * parameter_decl list
  | OldFuncDeclarator of declarator * string list
  | PtrDeclarator of spec_qual list * declarator

and parameter_decl =
  | NamedParamDecl of spec_qual list * declarator
  | AbsParamDecl of spec_qual list * (*abstract*)declarator

and s_typ = spec_qual list * (*abstract*)declarator

[@@deriving show { with_path = false }]

type init_declarator = declarator * s_expr option
[@@deriving show { with_path = false }]

type s_decl = spec_qual list * init_declarator list
[@@deriving show { with_path = false }]

type s_label =
  | S_OrdinaryLabel of string
  | S_CaseLabel of s_expr
  | S_DefaultLabel
[@@deriving show { with_path = false }]

type s_stmt =
  | S_NullStmt
  | S_LabelStmt of s_label * s_stmt
  | S_CompStmt of block_item list
  | S_ExprStmt of s_expr
  | S_IfStmt of s_expr * s_stmt
  | S_IfElseStmt of s_expr * s_stmt * s_stmt
  | S_SwitchStmt of s_expr * s_stmt
  | S_WhileStmt of s_expr * s_stmt
  | S_DoWhileStmt of s_stmt * s_expr
  | S_ForStmt1 of s_expr option * s_expr option * s_expr option * s_stmt
  | S_ForStmt2 of s_decl * s_expr option * s_expr option * s_stmt
  | S_GotoStmt of string
  | S_ContinueStmt
  | S_BreakStmt
  | S_ReturnStmt of s_expr option

and block_item =
  | DeclItem of s_decl
  | StmtItem of s_stmt

[@@deriving show { with_path = false }]

type s_func_def = spec_qual list * declarator * s_decl list * block_item list
[@@deriving show { with_path = false }]

type s_extern_decl =
  | S_FuncDef of s_func_def
  | S_Decl of s_decl
[@@deriving show { with_path = false }]
