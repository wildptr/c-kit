%{
open AST
%}

%token EOF
%token <string> Ident
%token <string> IntLit
%token <string> FloatLit
%token <string> CharLit
%token <string> StringLit
%token Ellipsis
%token PlusEq
%token MinusEq
%token StarEq
%token SlashEq
%token PercentEq
%token PipeEq
%token AndEq
%token CircEq
%token LtLtEq
%token GtGtEq
%token LtLt
%token GtGt
%token EqEq
%token BangEq
%token LtEq
%token GtEq
%token Eq
%token Lt
%token Gt
%token PlusPlus
%token MinusMinus
%token Arrow
%token Plus
%token Minus
%token Star
%token Slash
%token Percent
%token Bang
%token AndAnd
%token PipePipe
%token And
%token Pipe
%token Circ
%token Quest
%token Colon
%token Tilde
%token LBrace
%token RBrace
%token LBrack
%token RBrack
%token LParen
%token RParen
%token Semi
%token Comma
%token Dot
%token Hash

%token BOOL
%token BREAK
%token CASE
%token CHAR
%token CONST
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE
%token ELSE
%token EXTERN
%token FLOAT
%token FOR
%token GOTO
%token IF
%token INT
%token LONG
%token RETURN
%token SHORT
%token SIGNED
%token SIZEOF
%token STATIC
%token SWITCH
%token TYPEDEF
%token UNSIGNED
%token VOID
%token VOLATILE
%token WHILE

%nonassoc if_prec
%nonassoc ELSE

%start <AST.extern_decl list> translation_unit

%%

(* 6.5 *)
primary_expr:
| Ident         { IdentExpr $1 }
| IntLit        { LitExpr (AST.IntLit $1) }
| FloatLit      { LitExpr (AST.FloatLit $1) }
| CharLit       { LitExpr (AST.CharLit $1) }
| StringLit     { LitExpr (AST.StringLit $1) }
| LParen expr RParen { $2 }

postfix_expr:
| primary_expr {$1}
| postfix_expr LBrack expr RBrack { BinaryExpr (Index, $1, $3) }
| postfix_expr LParen separated_list(Comma, assign_expr) RParen
  { CallExpr ($1, $3) }
| postfix_expr Dot Ident        { UnaryExpr (Dot $3, $1) }
| postfix_expr Arrow Ident      { UnaryExpr (Arrow $3, $1) }
| postfix_expr PlusPlus         { UnaryExpr (PostInc, $1) }
| postfix_expr MinusMinus       { UnaryExpr (PostDec, $1) }

unary_expr:
| postfix_expr {$1}
| PlusPlus unary_expr           { UnaryExpr (PreInc, $2) }
| MinusMinus unary_expr         { UnaryExpr (PreDec, $2) }
| unary_operator cast_expr      { UnaryExpr ($1, $2) }
| SIZEOF unary_expr             { UnaryExpr (SizeOf, $2) }
| SIZEOF LParen type_name RParen { SizeOfExpr $3 }

unary_operator:
| And           { Addr }
| Star          { Deref }
| Plus          { AST.Plus }
| Minus         { AST.Minus }
| Tilde         { Not }
| Bang          { LogNot }

cast_expr:
| unary_expr {$1}
| LParen type_name RParen cast_expr { CastExpr ($2, $4) }

mult_expr:
| cast_expr {$1}
| mult_expr Star cast_expr      { BinaryExpr (Mul, $1, $3) }
| mult_expr Slash cast_expr     { BinaryExpr (Div, $1, $3) }
| mult_expr Percent cast_expr   { BinaryExpr (Mod, $1, $3) }

add_expr:
| mult_expr {$1}
| add_expr Plus mult_expr       { BinaryExpr (Add, $1, $3) }
| add_expr Minus mult_expr      { BinaryExpr (Sub, $1, $3) }

shift_expr:
| add_expr {$1}
| shift_expr LtLt add_expr      { BinaryExpr (LShift, $1, $3) }
| shift_expr GtGt add_expr      { BinaryExpr (RShift, $1, $3) }

rel_expr:
| shift_expr {$1}
| rel_expr Lt shift_expr        { BinaryExpr (Lt, $1, $3) }
| rel_expr Gt shift_expr        { BinaryExpr (Gt, $1, $3) }
| rel_expr LtEq shift_expr      { BinaryExpr (LtEq, $1, $3) }
| rel_expr GtEq shift_expr      { BinaryExpr (GtEq, $1, $3) }

eq_expr:
| rel_expr {$1}
| eq_expr EqEq rel_expr         { BinaryExpr (Eq, $1, $3) }
| eq_expr BangEq rel_expr       { BinaryExpr (NotEq, $1, $3) }

and_expr:
| eq_expr {$1}
| and_expr And eq_expr          { BinaryExpr (And, $1, $3) }

xor_expr:
| and_expr {$1}
| xor_expr Circ and_expr        { BinaryExpr (Xor, $1, $3) }

or_expr:
| xor_expr {$1}
| or_expr Pipe xor_expr         { BinaryExpr (Or, $1, $3) }

log_and_expr:
| or_expr {$1}
| log_and_expr AndAnd or_expr   { BinaryExpr (LogAnd, $1, $3) }

log_or_expr:
| log_and_expr {$1}
| log_or_expr PipePipe log_and_expr { BinaryExpr (LogOr, $1, $3) }

cond_expr:
| log_or_expr {$1}
| log_or_expr Quest expr Colon cond_expr { CondExpr ($1, $3, $5) }

assign_expr:
| cond_expr {$1}
| unary_expr assign_operator assign_expr { BinaryExpr ($2, $1, $3) }

assign_operator:
| Eq            { Assign }
| StarEq        { MulAssign }
| SlashEq       { DivAssign }
| PercentEq     { ModAssign }
| PlusEq        { AddAssign }
| MinusEq       { SubAssign }
| LtLtEq        { LShiftAssign }
| GtGtEq        { RShiftAssign }
| AndEq         { AndAssign }
| CircEq        { XorAssign }
| PipeEq        { OrAssign }

expr:
| assign_expr { $1 }
| expr Comma assign_expr { BinaryExpr (Seq, $1, $3) }

const_expr: cond_expr {$1}

(* 6.7 *)
declaration: decl_spec_list separated_list(Comma, init_declarator) Semi { $1, $2 }

decl_spec:
| sc_spec       { DeclStorageClass $1 }
| type_spec     { DeclTypeSpec $1 }
| type_qual     { DeclTypeQual $1 }

decl_spec_list: nonempty_list(decl_spec) {$1}

init_declarator:
| declarator    { $1, None }
| declarator Eq assign_expr { $1, Some $3 }

(* 6.7.1 *)
sc_spec:
| TYPEDEF       { Typedef }
| EXTERN        { Extern }
| STATIC        { Static }

(* 6.7.2 *)
type_spec:
| VOID          { Void }
| CHAR          { Char }
| BOOL          { Bool }
| SHORT         { Short }
| INT           { Int }
| LONG          { Long }
| FLOAT         { Float }
| DOUBLE        { Double }
| SIGNED        { Signed }
| UNSIGNED      { Unsigned }

(* 6.7.2.1 *)
spec_qual_list:
| type_spec loption(spec_qual_list) { TypeSpec $1 :: $2 }
| type_qual loption(spec_qual_list) { TypeQual $1 :: $2 }

(* 6.7.3 *)
type_qual:
| CONST         { Const }
| VOLATILE      { Volatile }

(* 6.7.5 *)
declarator:
| direct_declarator { $1 }
| pointer declarator { PtrDeclarator ($1, $2) }

direct_declarator:
| Ident { IdentDeclarator $1 }
| LParen declarator RParen { $2 }
| direct_declarator LBrack assign_expr RBrack { ArrayDeclarator ($1, $3) }
| direct_declarator LParen parameter_list RParen { FuncDeclarator ($1, $3) }
| direct_declarator LParen separated_list(Comma, Ident) RParen { OldFuncDeclarator ($1, $3) }

pointer: Star list(type_qual) { $2 }

parameter_list: separated_nonempty_list(Comma, parameter_decl) {$1}

parameter_decl:
| decl_spec_list declarator             { NamedParamDecl ($1, $2) }
| decl_spec_list                        { AbsParamDecl ($1, NullAbsDeclarator) }
| decl_spec_list abstract_declarator    { AbsParamDecl ($1, $2) }

(* 6.7.6 *)
type_name:
| spec_qual_list                        { $1, NullAbsDeclarator }
| spec_qual_list abstract_declarator    { $1, $2 }

abstract_declarator:
| pointer                       { PtrAbsDeclarator ($1, NullAbsDeclarator) }
| pointer abstract_declarator   { PtrAbsDeclarator ($1, $2) }
| direct_abstract_declarator    { $1 }

direct_abstract_declarator:
| LParen abstract_declarator RParen { $2 }
| direct_abstract_declarator LBrack assign_expr RBrack
  { ArrayAbsDeclarator ($1, $3) }
| direct_abstract_declarator LParen parameter_list RParen
  { FuncAbsDeclarator ($1, $3) }

(* 6.8 *)
stmt:
| labeled_stmt          { $1 }
| compound_stmt         { CompStmt $1 }
| expr_stmt             { $1 }
| selection_stmt        { $1 }
| iteration_stmt        { $1 }
| jump_stmt             { $1 }

(* 6.8.1 *)
labeled_stmt:
| Ident Colon stmt { LabelStmt ($1, $3) }
| CASE const_expr Colon stmt { CaseLabelStmt ($2, $4) }
| DEFAULT Colon stmt { DefaultLabelStmt $3 }

(* 6.8.2 *)
compound_stmt: LBrace list(block_item) RBrace {$2}

block_item:
| declaration   { DeclItem $1 }
| stmt          { StmtItem $1 }

(* 6.8.3 *)
expr_stmt:
| Semi          { NullStmt }
| expr Semi     { ExprStmt $1 }

(* 6.8.4 *)
selection_stmt:
| IF LParen expr RParen stmt
  %prec if_prec                         { IfStmt ($3, $5, NullStmt) }
| IF LParen expr RParen stmt ELSE stmt  { IfStmt ($3, $5, $7) }
| SWITCH LParen expr RParen stmt        { SwitchStmt ($3, $5) }

(* 6.8.5 *)
iteration_stmt:
| WHILE LParen expr RParen stmt         { WhileStmt ($3, $5) }
| DO stmt WHILE LParen expr RParen Semi { DoWhileStmt ($2, $5) }
| FOR LParen option(expr) Semi option(expr) Semi option(expr) RParen stmt
  { ForStmt1 ($3, $5, $7, $9) }
| FOR LParen declaration option(expr) Semi option(expr) RParen stmt
  { ForStmt2 ($3, $4, $6, $8) }

(* 6.8.6 *)
jump_stmt:
| GOTO Ident Semi               { GotoStmt $2 }
| CONTINUE Semi                 { ContinueStmt }
| BREAK Semi                    { BreakStmt }
| RETURN option(expr) Semi      { ReturnStmt $2 }

(* 6.9 *)
translation_unit: list(extern_decl) EOF {$1}

extern_decl:
| func_def      { $1 }
| declaration   { Decl $1 }

func_def: decl_spec_list declarator list(declaration) compound_stmt
  { FuncDef ($1, $2, $3, $4) }
