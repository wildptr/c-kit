type loc = int * int

type 'a node = { body: 'a; loc: loc }

(* for types and declarators *)

type storage_spec =
  | Typedef
  | Extern
  | Static
  | Auto
  | Register

type func_spec =
  | Inline

type type_qual = Const | Volatile | Restrict

(* for expressions *)

type unop = NEG | NOT | LOGIC_NOT | OPLUS | OMINUS

type binop =
  | MUL | DIV | MOD | ADD | SUB
  | LSHIFT | RSHIFT
  | LESS | GREATER | LESS_EQ | GREATER_EQ | EQUAL | NOT_EQUAL
  | AND | XOR | OR
  | LOGIC_AND | LOGIC_OR

type const_type = Const_Int | Const_Float | Const_Char

type inc_dec = PRE_INC | PRE_DEC | POST_INC | POST_DEC

(* ======================================================================== *)
(* types, expressions, declarators *)

type ptype = (type_spec list * type_qual list) * declarator node option

and pexpr =
  | PE_Const of const_type * string
  | PE_String of string
  | PE_Ident of string
  | PE_Paren of pexpr node
  | PE_Unary of unop * pexpr node
  | PE_Binary of binop * pexpr node * pexpr node
  | PE_Index of pexpr node * pexpr node
  | PE_Call of pexpr node * pexpr node array
  | PE_Dot of pexpr node * string node
  | PE_Arrow of pexpr node * string node
  | PE_IncDec of inc_dec * pexpr node
  | PE_SizeofE of pexpr node
  | PE_SizeofT of ptype node
  | PE_Addr of pexpr node
  | PE_Deref of pexpr node
  | PE_Cast of ptype node * pexpr node
  | PE_Cond of pexpr node * pexpr node * pexpr node
  | PE_Assign of pexpr node * pexpr node
  | PE_Assign_Binary of binop * pexpr node * pexpr node
  | PE_Seq of pexpr node * pexpr node

and array_declarator_attr =
  { ad_size_opt : pexpr node option;
    ad_type_qual : type_qual list;
    ad_static_flag : bool }

and declarator =
  | D_Base of string
  | D_Ptr of declarator node * type_qual list
  | D_Array of declarator node * array_declarator_attr
  | D_Func of declarator node * param_decl list * bool(*vararg*)
  | D_Old_Func of declarator node * string list
  | D_Paren of declarator node

and param_decl = decl_spec node * declarator node

and su_spec =
  | SU0 of string node
  | SU1 of string node option * struct_decl list

and enumerator = string node * pexpr node option

and enum_spec =
  | Enum0 of string node
  | Enum1 of string node option * enumerator list

and type_spec =
  | Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Signed
  | Unsigned
  | Bool
  | Complex
  | Struct of su_spec
  | Union of su_spec
  | Enum of enum_spec
  | Typedef_Name of string

and decl_spec =
  { ds_stor : storage_spec list;
    ds_type_spec : type_spec list;
    ds_type_qual : type_qual list;
    ds_func_spec : func_spec list }

(* The standard says specifier-qualifier-list rather than
   declaration-specifiers *)
and struct_decl = decl_spec node * struct_declarator list

and struct_declarator = declarator node option * pexpr node option

(* ======================================================================== *)

let rec declarator_name = function
  | D_Base name -> name
  | D_Ptr (d, _) | D_Array (d, _) | D_Func (d, _, _) | D_Old_Func (d, _) | D_Paren d ->
    declarator_name d.body

type designator =
  | Desig_None
  | Desig_Index of pexpr node
  | Desig_Field of string node

type initializer_ =
  | Init_Expr of pexpr node
  | Init_List of (designator list node * initializer_) list node

type init_declarator = declarator node * initializer_ option

type decl = decl_spec node * init_declarator list

(** Statements **)

type label =
  | Label of string
  | Label_Case of pexpr node
  | Label_Default

type pstmt =
  | PS_Null
  | PS_Expr of pexpr node
  | PS_Block of block_item list
  | PS_Goto of string node
  | PS_Continue
  | PS_Break
  | PS_Return of pexpr node option
  | PS_Labeled of pstmt node * label
  | PS_If of pexpr node * pstmt node
  | PS_Ifelse of pexpr node * pstmt node * pstmt node
  | PS_Switch of pexpr node * pstmt node
  | PS_While of pexpr node * pstmt node
  | PS_Do_While of pstmt node * pexpr node
  | PS_For1 of pexpr node option * pexpr node option * pexpr node option * pstmt node
  | PS_For2 of decl * pexpr node option * pexpr node option * pstmt node

and block_item =
  | Item_Stmt of pstmt node
  | Item_Decl of decl

(** External definitions **)

type func_def =
  { fd_decl_spec : decl_spec node;
    fd_declarator : declarator node;
    fd_oldstyle_param_decl : decl list;
    fd_body : pstmt node }

type extdef =
  | Func_Def of func_def
  | Decl of decl
