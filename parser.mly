%{
open Syntax

let node body (pos0, pos1) =
  let open Lexing in
  let abspos0 = pos0.pos_bol + pos0.pos_cnum
  and abspos1 = pos1.pos_bol + pos1.pos_cnum in
  { body; loc = (abspos0, abspos1) }

let empty_decl_spec =
  { ds_stor=[]; ds_type_spec=[]; ds_type_qual=[]; ds_func_spec=[] }

let mk_func_def (fd_decl_spec, fd_declarator, fd_oldstyle_param_decl, fd_body) =
  { fd_decl_spec; fd_declarator; fd_oldstyle_param_decl; fd_body }

%}

%token EOF

%token <string list> (* no_expand_list *) PREIDENT
%token DIRECTIVE
%token HASHHASH
%token HASH
%token UNKNOWN

%token <string> IDENT
%token <string> TYPEIDENT
%token <string> INT_LIT
%token <string> FLOAT_LIT
%token <string> STRING_LIT
%token <string> CHAR_LIT

%token ELLIPSIS
%token PLUSEQ
%token MINUSEQ
%token STAREQ
%token SLASHEQ
%token PERCENTEQ
%token BAREQ
%token AMPEQ
%token CIRCEQ
%token LTLTEQ
%token GTGTEQ
%token LTLT
%token GTGT
%token EQEQ
%token BANGEQ
%token LTEQ
%token GTEQ
%token EQ
%token LT
%token GT
%token PLUSPLUS
%token MINUSMINUS
%token ARROW
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token BANG
%token AMPAMP
%token BARBAR
%token AMP
%token BAR
%token CIRC
%token QUEST
%token COLON
%token TILDE
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token SEMI
%token COMMA
%token DOT

%token AUTO
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
%token ENUM
%token EXTERN
%token FLOAT
%token FOR
%token GOTO
%token IF
%token INLINE
%token INT
%token LONG
%token REGISTER
%token RESTRICT
%token RETURN
%token SHORT
%token SIGNED
%token SIZEOF
%token STATIC
%token STRUCT
%token SWITCH
%token TYPEDEF
%token UNION
%token UNSIGNED
%token VOID
%token VOLATILE
%token WHILE

%left BARBAR
%left AMPAMP
%left BAR
%left CIRC
%left AMP
%left EQEQ BANGEQ
%left LT GT LTEQ GTEQ
%left LTLT GTGT
%left PLUS MINUS
%left STAR SLASH PERCENT

%type <param_decl> param_decl
%type <initializer_> init
%type <designator list node * initializer_> init_list_item

%start <extdef list> translation_unit

%%

any_ident:
| IDENT {$1}
| TYPEIDENT {$1}

ident_loc: any_ident { node $1 $loc }

(* *_expr_noid never expands to IDENT *)

(* 6.4.4
  constant:
    integer-constant
    floating-constant
    enumeration-constant
    character-constant
 *)

(** Expressions **)

(* 6.5.1
  primary-expression:
    identifier
    constant
    string-literal
    '(' expression ')'
 *)
lit_expr:
| INT_LIT
  { node (PE_Const (Const_Int, $1)) $loc }
| FLOAT_LIT
  { node (PE_Const (Const_Float, $1)) $loc }
| CHAR_LIT
  { node (PE_Const (Const_Char, $1)) $loc }
| STRING_LIT
  { node (PE_String $1) $loc }

prim_expr_noid:
| lit_expr {$1}
| LPAREN expr_noid RPAREN
  { node (PE_Paren $2) $loc }
| LPAREN IDENT RPAREN
  { node (PE_Ident $2) $loc }

(*
prim_expr:
| prim_expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }
*)

prim_expr_cast:
| lit_expr {$1}
| IDENT { node (PE_Ident $1) $loc }

(* 6.5.2
  postfix-expression:
    primary-expression
    postfix-expression '[' expression ']'
    postfix-expression '(' argument-expression-list? ')'
    postfix-expression '.' identifier
    postfix-expression '->' identifier
    postfix-expression '++'
    postfix-expression '--'
    '( type-name ')' '{' initializer-list '}'

  argument-expression-list:
    assignment-expression
    argument-expression-list ',' assignment-expression
 *)
postfix_expr_noid:
| prim_expr_noid {$1}
| postfix_expr LBRACK expr RBRACK
  { node (PE_Index ($1, $3)) $loc }
| postfix_expr LPAREN separated_list(COMMA, assign_expr) RPAREN
  { node (PE_Call ($1, Array.of_list $3)) $loc }
| postfix_expr DOT ident_loc
  { node (PE_Dot ($1, $3)) $loc }
| postfix_expr ARROW ident_loc
  { node (PE_Arrow ($1, $3)) $loc }
| postfix_expr PLUSPLUS
  { node (PE_IncDec (POST_INC, $1)) $loc }
| postfix_expr MINUSMINUS
  { node (PE_IncDec (POST_DEC, $1)) $loc }

postfix_expr:
| postfix_expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }

postfix_expr_cast:
| prim_expr_cast {$1}
| postfix_expr_cast LBRACK expr RBRACK
  { node (PE_Index ($1, $3)) $loc }
| postfix_expr_cast DOT ident_loc
  { node (PE_Dot ($1, $3)) $loc }
| postfix_expr_cast ARROW ident_loc
  { node (PE_Arrow ($1, $3)) $loc }
| postfix_expr_cast PLUSPLUS
  { node (PE_IncDec (POST_INC, $1)) $loc }
| postfix_expr_cast MINUSMINUS
  { node (PE_IncDec (POST_DEC, $1)) $loc }

(* 6.5.3
  unary-expression:
    postfix-expression
    '++' unary-expression
    '--' unary-expression
    unary-operator cast-expression
    'sizeof' unary-expression
    'sizeof' '(' type-name ')'

  unary-operator:
    '&' | '*' | '+' | '-' | '~' | '!'
 *)
unary_expr_common:
| SIZEOF unary_expr
  { node (PE_SizeofE $2) $loc }
| SIZEOF LPAREN typ RPAREN
  { node (PE_SizeofT $3) $loc }
| TILDE unary_expr
  { node (PE_Unary (NOT, $2)) $loc }
| BANG unary_expr
  { node (PE_Unary (LOGIC_NOT, $2)) $loc }

unary_expr_cast:
| postfix_expr_cast {$1}
| unary_expr_common {$1}

unary_expr_noid:
| postfix_expr_noid {$1}
| unary_expr_common {$1}
| PLUSPLUS unary_expr
  { node (PE_IncDec (PRE_INC, $2)) $loc }
| MINUSMINUS unary_expr
  { node (PE_IncDec (PRE_DEC, $2)) $loc }
| AMP unary_expr
  { node (PE_Addr $2) $loc }
| STAR unary_expr
  { node (PE_Deref $2) $loc }
| PLUS unary_expr
  { node (PE_Unary (OPLUS, $2)) $loc }
| MINUS unary_expr
  { node (PE_Unary (OMINUS, $2)) $loc }

unary_expr:
| unary_expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }

(* 6.5.4
  cast-expression:
    unary-expression
    '(' type-name ')' cast-expression
 *)
cast_expr_noid:
| unary_expr_noid {$1}
| LPAREN typ RPAREN cast_expr
  { node (PE_Cast ($2, $4)) $loc }
| LPAREN IDENT RPAREN unary_expr_cast
  { let typ = node ([Typedef_Name $2], []) $loc($2) in
    node (PE_Cast (typ, $4)) $loc }

cast_expr:
| cast_expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }

(* 6.5.5
  multiplicative-expression:
    cast-expression
    multiplicative-expression '*' cast-expression
    multiplicative-expression '/' cast-expression
    multiplicative-expression '%' cast-expression
 *)

(* 6.5.6
  additive-expression:
    multiplicative-expression
    additive-expression '+' multiplicative-expression
    additive-expression '-' multiplicative-expression
 *)

(* ... *)

(* logical-OR-expression *)
bin_expr_noid:
| cast_expr_noid {$1}
| bin_expr STAR    bin_expr { node (PE_Binary (MUL,        $1, $3)) $loc }
| bin_expr SLASH   bin_expr { node (PE_Binary (DIV,        $1, $3)) $loc }
| bin_expr PERCENT bin_expr { node (PE_Binary (MOD,        $1, $3)) $loc }
| bin_expr PLUS    bin_expr { node (PE_Binary (ADD,        $1, $3)) $loc }
| bin_expr MINUS   bin_expr { node (PE_Binary (SUB,        $1, $3)) $loc }
| bin_expr LTLT    bin_expr { node (PE_Binary (LSHIFT,     $1, $3)) $loc }
| bin_expr GTGT    bin_expr { node (PE_Binary (RSHIFT,     $1, $3)) $loc }
| bin_expr LT      bin_expr { node (PE_Binary (LESS,       $1, $3)) $loc }
| bin_expr GT      bin_expr { node (PE_Binary (GREATER,    $1, $3)) $loc }
| bin_expr LTEQ    bin_expr { node (PE_Binary (LESS_EQ,    $1, $3)) $loc }
| bin_expr GTEQ    bin_expr { node (PE_Binary (GREATER_EQ, $1, $3)) $loc }
| bin_expr EQEQ    bin_expr { node (PE_Binary (EQUAL,      $1, $3)) $loc }
| bin_expr BANGEQ  bin_expr { node (PE_Binary (NOT_EQUAL,  $1, $3)) $loc }
| bin_expr AMP     bin_expr { node (PE_Binary (AND,        $1, $3)) $loc }
| bin_expr CIRC    bin_expr { node (PE_Binary (XOR,        $1, $3)) $loc }
| bin_expr BAR     bin_expr { node (PE_Binary (OR,         $1, $3)) $loc }
| bin_expr AMPAMP  bin_expr { node (PE_Binary (LOGIC_AND,  $1, $3)) $loc }
| bin_expr BARBAR  bin_expr { node (PE_Binary (LOGIC_OR,   $1, $3)) $loc }

bin_expr:
| bin_expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }

(* 6.5.15
  conditional-expression:
    logical-OR-expression
    logical-OR-expression '?' expression ':' conditional-expression
 *)
cond_expr_noid:
| bin_expr_noid {$1}
| bin_expr QUEST expr COLON cond_expr
  { node (PE_Cond ($1, $3, $5)) $loc }

cond_expr:
| cond_expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }

(* 6.5.16
  assignment-expression:
    conditional-expression
    unary-expression assignment-operator assignment-expression

  assignment-operator:
    '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' '|='
 *)
assign_expr_noid:
| cond_expr_noid {$1}
| unary_expr EQ assign_expr
  { node (PE_Assign ($1, $3)) $loc }
| unary_expr assign_op assign_expr
  { node (PE_Assign_Binary ($2, $1, $3)) $loc }

assign_expr:
| assign_expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }

assign_op:
| STAREQ    { MUL }
| SLASHEQ   { DIV }
| PERCENTEQ { MOD }
| PLUSEQ    { ADD }
| MINUSEQ   { SUB }
| LTLTEQ    { LSHIFT }
| GTGTEQ    { RSHIFT }
| AMPEQ     { AND }
| CIRCEQ    { XOR }
| BAREQ     { OR }

(* 6.5.17
  expression:
    assignment-expression
    expression ',' assignment-expression
 *)
expr_noid:
| assign_expr_noid {$1}
| expr COMMA assign_expr
  { node (PE_Seq ($1, $3)) $loc }

expr:
| expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }

(* 6.6
  constant-expression:
    conditional-expression
 *)
%inline const_expr: cond_expr {$1}

(** Declarations **)

(* 6.7
  declaration:
    declaration-specifiers init-declarator-list? ';'

  declaration-specifiers:
    storage-class-specifier declaration-specifiers?
    type-specifier declaration-specifiers?
    type-qualifier declaration-specifiers?
    function-specifier declaration-specifiers?

  init-declarator-list:
    init-declarator
    init-declarator-list ',' init-declarator

  init-declarator:
    declarator
    declarator '=' initializer
 *)
decl:
| s=decl_spec hd=init_declr tl=list(preceded(COMMA, init_declr)) SEMI
| s=fake_decl_spec hd=init_declr tl=list(preceded(COMMA, init_declr)) SEMI
| s=incomplete_decl_spec hd=init_declr_1not_tyid tl=list(preceded(COMMA, init_declr)) SEMI
  { (node s $loc(s), hd::tl) }
| s1=incomplete_decl_spec_opt name=IDENT hd=init_declr_1any_id tl=list(preceded(COMMA, init_declr)) SEMI
  { let s = { s1 with ds_type_spec = [Typedef_Name name] }
    in (node s ($startpos(s1), $endpos(name)), hd::tl) }
| name=IDENT s2=incomplete_decl_spec hd=init_declr tl=list(preceded(COMMA, init_declr)) SEMI
  { let s = { s2 with ds_type_spec = [Typedef_Name name] }
    in (node s ($startpos(name), $endpos(s2)), hd::tl) }

incomplete_decl_spec: (* those that do not contain a type specifier *)
| storage_spec
  { { ds_stor = [$1];
      ds_type_spec = [];
      ds_type_qual = [];
      ds_func_spec = [] } }
| type_qual
  { { ds_stor = [];
      ds_type_spec = [];
      ds_type_qual = [$1];
      ds_func_spec = [] } }
| func_spec
  { { ds_stor = [];
      ds_type_spec = [];
      ds_type_qual = [];
      ds_func_spec = [$1] } }
| storage_spec incomplete_decl_spec
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor = $1::ds_stor;
      ds_type_spec;
      ds_type_qual;
      ds_func_spec } }
| type_qual incomplete_decl_spec
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec;
      ds_type_qual = $1::ds_type_qual;
      ds_func_spec } }
| func_spec incomplete_decl_spec
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec;
      ds_type_qual;
      ds_func_spec = $1::ds_func_spec } }

decl_spec_notyid:
| type_spec_notyid
  { { ds_stor = [];
      ds_type_spec = [$1];
      ds_type_qual = [];
      ds_func_spec = [] } }
| storage_spec decl_spec_notyid
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor = $1::ds_stor;
      ds_type_spec;
      ds_type_qual;
      ds_func_spec } }
| type_spec_notyid incomplete_decl_spec
| type_spec_notyid decl_spec_notyid
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec = $1::ds_type_spec;
      ds_type_qual;
      ds_func_spec } }
| type_qual decl_spec_notyid
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec;
      ds_type_qual = $1::ds_type_qual;
      ds_func_spec } }
| func_spec decl_spec_notyid
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec;
      ds_type_qual;
      ds_func_spec = $1::ds_func_spec } }

%inline (* inlining avoids conflicts *)
incomplete_decl_spec_opt:
| incomplete_decl_spec {$1}
| { empty_decl_spec }

decl_spec_tyid:
| type_spec_tyid
  { { ds_stor = [];
      ds_type_spec = [$1];
      ds_type_qual = [];
      ds_func_spec = [] } }
| storage_spec decl_spec_tyid
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor = $1::ds_stor;
      ds_type_spec;
      ds_type_qual;
      ds_func_spec } }
| type_spec_tyid incomplete_decl_spec
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec = $1::ds_type_spec;
      ds_type_qual;
      ds_func_spec } }
| type_qual decl_spec_tyid
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec;
      ds_type_qual = $1::ds_type_qual;
      ds_func_spec } }
| func_spec decl_spec_tyid
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec;
      ds_type_qual;
      ds_func_spec = $1::ds_func_spec } }

decl_spec: decl_spec_notyid | decl_spec_tyid {$1}

%inline (* avoid conflicts *)
fake_decl_spec:
  s1=incomplete_decl_spec name=IDENT s2=incomplete_decl_spec
  { { ds_stor = s1.ds_stor @ s2.ds_stor;
      ds_type_spec = [Typedef_Name name];
      ds_type_qual = s1.ds_type_qual @ s2.ds_type_qual;
      ds_func_spec = s1.ds_func_spec @ s2.ds_func_spec } }

(* This equivalent definition causes conflicts.
fake_decl_spec:
| storage_spec fake_decl_spec
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor = $1::ds_stor;
      ds_type_spec;
      ds_type_qual;
      ds_func_spec } }
| IDENT incomplete_decl_spec
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec = Typedef_Name $1 :: ds_type_spec;
      ds_type_qual;
      ds_func_spec } }
| type_qual fake_decl_spec
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec;
      ds_type_qual = $1::ds_type_qual;
      ds_func_spec } }
| func_spec fake_decl_spec
  { let { ds_stor; ds_type_spec; ds_type_qual; ds_func_spec } = $2 in
    { ds_stor;
      ds_type_spec;
      ds_type_qual;
      ds_func_spec = $1::ds_func_spec } } *)

init_declr:
| declr
  { ($1, None) }
| declr EQ init
  { ($1, Some $3) }

init_declr_1not_tyid:
| declr_1not_tyid
  { ($1, None) }
| declr_1not_tyid EQ init
  { ($1, Some $3) }

init_declr_1any_id:
| declr_1any_id
  { ($1, None) }
| declr_1any_id EQ init
  { ($1, Some $3) }

(* 6.7.1
  storage-class-specifier:
    'typedef'
    'extern'
    'static'
    'auto'
    'register'
 *)
storage_spec:
| TYPEDEF  { Typedef }
| EXTERN   { Extern }
| STATIC   { Static }
| AUTO     { Auto }
| REGISTER { Register }

(* 6.7.2
  type-specifier:
    'void'
    'char'
    'short'
    'int'
    'long'
    'float'
    'double'
    'signed'
    'unsigned'
    '_Bool'
    '_Complex'
    struct-or-union-specifier
    enum-specifier
    typedef-name
 *)
type_spec_notyid:
| VOID     { Void }
| CHAR     { Char }
| SHORT    { Short }
| INT      { Int }
| LONG     { Long }
| FLOAT    { Float }
| DOUBLE   { Double }
| SIGNED   { Signed }
| UNSIGNED { Unsigned }
| BOOL     { Bool }

type_spec_tyid:
| TYPEIDENT { Typedef_Name $1 }

type_spec: type_spec_notyid | type_spec_tyid {$1}

(* 6.7.2.1
  struct-or-union-specifier:
    struct-or-union identifier? '{' struct-declaration-list '}'
    struct-or-union identifier

  struct-or-union:
    'struct'
    'union'

  struct-declaration-list:
    struct-declaration
    struct-declaration-list strut-declaration

  struct-declaration:
    specifier-qualifier-list struct-declarator-list ';'

  specifier-qualifier-list:
    type-specifier specifier-qualifier-list?
    type-qualifier specifier-qualifier-list?

  struct-declarator-list:
    struct-declarator
    struct-declarator-list ',' struct-declarator

  struct-declarator:
    declarator
    declarator? ':' constant-expression
 *)
spec_qual_list:
| type_spec
  { ([$1],[]) }
| type_qual
  { ([],[$1]) }
| type_spec spec_qual_list
  { let (s,q) = $2 in
    ($1::s, q) }
| type_qual spec_qual_list
  { let (s,q) = $2 in
    (s, $1::q) }

spec_qual_list_aug:
| spec_qual_list {$1}
| IDENT nonempty_list(type_qual)
  { ([Typedef_Name $1], $2) }
| q1=nonempty_list(type_qual) name=IDENT q2=list(type_qual)
  { ([Typedef_Name name], q1@q2) }

(* 6.7.2.2
  enum-specifier:
    'enum' identifier? '{' enumerator-list '}'
    'enum' identifier? '{' enumerator-list ',' '}'
    'enum' identifier

  enumerator-list:
    enumerator
    enumerator-list ',' enumerator

  enumerator:
    enumeration-constant
    enumeration-constant '=' constant-expression
 *)

(* 6.7.3
  type-qualifier:
    'const'
    'restrict'
    'volatile'
 *)
type_qual:
| CONST { Const }
| RESTRICT { Restrict }
| VOLATILE { Volatile }

(* 6.7.4
  function-specifier:
    'inline'
 *)
func_spec:
| INLINE { Inline }

(* 6.7.5
  declarator:
    pointer? direct-declarator

  direct-declarator:
    identifier
    '(' declarator ')'
    direct-declarator '[' type-qualifier-list? assignment-expression? ']'
    direct-declarator '[' 'static' type-qualifier-list? assignment-expression ']'
    direct-declarator '[' type-qualifier-list 'static' assignment-expression ']'
    direct-declarator '[' type-qualifier-list? '*' ']'
    direct-declarator '(' parameter-type-list ')'
    direct-declarator '(' identifier-list? ')'

  pointer:
    '*' type-qualifier-list?
    '*' type-qualifier-list? pointer

  type-qualifier-list:
    type-qualifier
    type-qualifier-list type-qualifier

  parameter-type-list:
    parameter-list
    parameter-list ',' '...'

  parameter-list:
    parameter-declaration
    parameter-list ',' parameter-declaration

  parameter-declaration:
    declaration-specifiers declarator
    declaration-specifiers abstract-declarator?

  identifier-list:
    identifier
    identifier-list ',' identifier
 *)
declr:
| direct_declr {$1}
| ptr direct_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }

declr_1not_tyid: (* first terminal is not TYPEIDENT *)
| direct_declr_1not_tyid {$1}
| ptr direct_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }

declr_1any_id:
| IDENT | TYPEIDENT
  { node (D_Base $1) $loc }
| declr_1any_id declr_suffix
  { node ($2 $1) $loc }

func_declr_suffix_ansi:
| LPAREN param_type_list RPAREN
  { let (decls, vararg) = $2 in
    fun d -> D_Func (d, decls, vararg) }
| LPAREN RPAREN
  { fun d -> D_Old_Func (d, []) }

func_declr_suffix_oldstyle:
  LPAREN hd=IDENT tl=list(preceded(COMMA, any_ident)) RPAREN
  { fun d -> D_Old_Func (d, hd::tl) }

abs_declr_suffix:
| LBRACK q=list(type_qual) e=assign_expr? RBRACK
  { fun d -> D_Array (d, {ad_size_opt=e; ad_type_qual=q; ad_static_flag=false}) }
| LBRACK STATIC q=list(type_qual) e=assign_expr RBRACK
| LBRACK q=nonempty_list(type_qual) STATIC e=assign_expr RBRACK
  { fun d -> D_Array (d, {ad_size_opt=Some e; ad_type_qual=q; ad_static_flag=true}) }
| LBRACK q=list(type_qual) STAR RBRACK
  { fun d -> D_Array (d, {ad_size_opt=None; ad_type_qual=q; ad_static_flag=false}) }
| func_declr_suffix_ansi { $1 }

declr_suffix:
| abs_declr_suffix
| func_declr_suffix_oldstyle
  { $1 }

(*
  int f(int(T))
  => int f(int _(T _))  (T is typedef-name)
  => int f(int T)       (T is not typedef-name)
 *)
direct_declr:
| IDENT | TYPEIDENT
  { node (D_Base $1) $loc }
| LPAREN declr RPAREN
  { node (D_Paren $2) $loc }
| direct_declr declr_suffix
  { node ($2 $1) $loc }

direct_declr_1not_tyid:
| IDENT
  { node (D_Base $1) $loc }
| LPAREN declr RPAREN
  { node (D_Paren $2) $loc }
| direct_declr_1not_tyid declr_suffix
  { node ($2 $1) $loc }

param_type_list:
| param_decl
  { ([$1], false) }
| param_decl COMMA ELLIPSIS
  { ([$1], true) }
| param_decl COMMA param_type_list
  { let decls, vararg = $3 in
    ($1::decls, vararg) }

(*
  int f(int(T()))
  => int f(int _(T _()))        (T is typedef-name)
  => int f(int T())             (T is not typedef-name)
 *)
param_decl:
| decl_spec abs_declr
| incomplete_decl_spec abs_declr_1not_any_id (* missing type specifier *)
  { (node $1 $loc($1), $2) }
| s1=incomplete_decl_spec_opt name=IDENT s2=incomplete_decl_spec_opt d=abs_declr
  { let s =
      { ds_stor = s1.ds_stor @ s2.ds_stor;
        ds_type_spec = [Typedef_Name name];
        ds_type_qual = s1.ds_type_qual @ s2.ds_type_qual;
        ds_func_spec = s1.ds_func_spec @ s2.ds_func_spec }
    in (node s ($startpos(s1), $endpos(s2)), d) }
| decl_spec
  { (node $1 $loc($1), node (D_Base "") ($endpos, $endpos)) }

ptr:
| STAR q=list(type_qual)
  { [(q, $startpos)] }
| STAR q=list(type_qual) p=ptr
  { (q, $startpos) :: p }

(* 6.7.6
  type-name:
    specifier-qualifier-list abstract-declarator?
  abstract-declarator:
    pointer
    pointer? direct-abstract-declarator
  direct-abstract-declarator:
    '(' abstract-declarator ')'
    direct-abstract-declarator? '[' type-qualifier-list? assignment-expression? ']'
    direct-abstract-declarator? '[' 'static' type-qualifier-list? assignment-expression ']'
    direct-abstract-declarator? '[' type-qualifier-list 'static' assignment-expression ']'
    direct-abstract-declarator? '[' '*' ']'
    direct-abstract-declarator? '(' parameter-type-list? ')'
 *)
typ:
| spec_qual_list_aug
  { node $1 $loc }

abs_declr: direct_abs_declr | indirect_abs_declr {$1}

abs_declr_1not_any_id: direct_abs_declr_1not_any_id | indirect_abs_declr {$1}

indirect_abs_declr:
| ptr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 (node (D_Base "") ($endpos, $endpos)) }
| ptr direct_abs_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }

(*
    int f(int(T));
 *)

direct_abs_declr:
| IDENT | TYPEIDENT
  { node (D_Base ($1)) $loc }
| LPAREN enclosed_abs_declr RPAREN
  { node (D_Paren $2) $loc }
| abs_declr_suffix
  { node ($1 (node (D_Base "") ($endpos, $endpos))) $loc }
| direct_abs_declr abs_declr_suffix
  { node ($2 $1) $loc }

direct_abs_declr_1not_any_id:
| LPAREN enclosed_abs_declr RPAREN
  { node (D_Paren $2) $loc }
| abs_declr_suffix
  { node ($1 (node (D_Base "") ($endpos, $endpos))) $loc }
| direct_abs_declr_1not_any_id abs_declr_suffix
  { node ($2 $1) $loc }

enclosed_abs_declr:
| indirect_abs_declr
| enclosed_abs_declr_aux
  { $1 }
| IDENT
  { node (D_Base ($1)) $loc }

enclosed_abs_declr_aux:
| LPAREN d=enclosed_abs_declr RPAREN
  { node (D_Paren d) $loc }
| d=abs_declr_suffix
  { node (d (node (D_Base "") ($endpos, $endpos))) $loc }
| enclosed_abs_declr_aux abs_declr_suffix
  { node ($2 $1) $loc }

(* 6.7.7
  typedef-name:
    identifier
 *)

(* 6.7.8
  initializer:
    assignment-expression
    '{' initializer-list '}'
    '{' initializer-list ',' '}'

  initializer-list:
    designation? initializer
    initializer-list ',' designation? initializer

  designation:
    designator-list '='

  designator-list:
    designator
    designator-list designator

  designator:
    '[' constant-expression ']'
    '.' identifier
 *)
init:
| assign_expr
  { Init_Expr $1 }
| LBRACE separated_nonempty_list(COMMA, init_list_item) RBRACE
| LBRACE nonempty_list(terminated(init_list_item, COMMA)) RBRACE
  { Init_List (node $2 $loc) }

init_list_item:
| init
  { (node [] ($startpos, $startpos), $1) }
| designation init
  { ($1, $2) }

designation: nonempty_list(designator) EQ { node $1 $loc }

designator:
| LBRACK const_expr RBRACK
  { Desig_Index $2 }
| DOT ident_loc
  { Desig_Field $2 }

(** Statements **)

(* 6.8
  statement:
    labeled-statement
    compound-statement
    expression-statement
    selection-statement
    iteration-statement
    jump-statement
 *)
stmt:
| semicolon_terminated_stmt
| comp_stmt
| stmt_terminated_stmt
  {$1}

stmt_before_else:
| semicolon_terminated_stmt
| comp_stmt
| stmt_terminated_stmt_before_else
  {$1}

(* 6.8.1
  labeled-statement:
    identifier ':' statement
    'case' constant-expression ':' statement
    'default' ':' statement
 *)

(* 6.8.2
  compound-statement:
    '{' block-item-list? '}'

  block-item-list:
    block-item
    block-item-list block-item

  block-item:
    declaration
    statement
 *)
comp_stmt: LBRACE list(block_item) RBRACE { node (PS_Block $2) $loc }

block_item:
| decl { Item_Decl $1 }
| stmt { Item_Stmt $1 }

(* 6.8.3
  expression-stmt:
    expression? ';'
 *)
semicolon_terminated_stmt:
| expr SEMI
  { node (PS_Expr $1) $loc }
| SEMI
  { node PS_Null $loc }
| DO s=stmt WHILE LPAREN e=expr RPAREN SEMI
  { node (PS_Do_While (s, e)) $loc }
| GOTO ident_loc SEMI
  { node (PS_Goto $2) $loc }
| CONTINUE SEMI
  { node PS_Continue $loc }
| BREAK SEMI
  { node PS_Break $loc }
| RETURN expr? SEMI
  { node (PS_Return $2) $loc }

stmt_terminated_stmt:
(* labeled *)
| name=any_ident COLON s=stmt
  { node (PS_Labeled (s, Label name)) $loc }
| CASE e=const_expr COLON s=stmt
  { node (PS_Labeled (s, Label_Case e)) $loc }
| DEFAULT COLON s=stmt
  { node (PS_Labeled (s, Label_Default)) $loc }
(* selection *)
| IF LPAREN e=expr RPAREN s=stmt
  { node (PS_If (e, s)) $loc }
| IF LPAREN e=expr RPAREN s1=stmt_before_else ELSE s2=stmt
  { node (PS_Ifelse (e, s1, s2)) $loc }
| SWITCH LPAREN e=expr RPAREN s=stmt
  { node (PS_Switch (e, s)) $loc }
(* iteration *)
| WHILE LPAREN e=expr RPAREN s=stmt
  { node (PS_While (e, s)) $loc }
| FOR LPAREN e1=expr? SEMI e2=expr? SEMI e3=expr? RPAREN s=stmt
  { node (PS_For1 (e1, e2, e3, s)) $loc }
| FOR LPAREN d=decl e2=expr? SEMI e3=expr? RPAREN s=stmt
  { node (PS_For2 (d, e2, e3, s)) $loc }

(* 6.8.4
  selection-statement:
    'if' '(' expression ')' statement
    'if' '(' expression ')' statement 'else' statement
    'switch' '(' expression ')' statement
 *)

stmt_terminated_stmt_before_else:
(* labeled *)
| name=any_ident COLON s=stmt_before_else
  { node (PS_Labeled (s, Label name)) $loc }
| CASE e=const_expr COLON s=stmt_before_else
  { node (PS_Labeled (s, Label_Case e)) $loc }
| DEFAULT COLON s=stmt_before_else
  { node (PS_Labeled (s, Label_Default)) $loc }
(* selection *)
| IF LPAREN e=expr RPAREN s1=stmt_before_else ELSE s2=stmt_before_else
  { node (PS_Ifelse (e, s1, s2)) $loc }
| SWITCH LPAREN e=expr RPAREN s=stmt_before_else
  { node (PS_Switch (e, s)) $loc }
(* iteration *)
| WHILE LPAREN e=expr RPAREN s=stmt_before_else
  { node (PS_While (e, s)) $loc }
| FOR LPAREN e1=expr? SEMI e2=expr? SEMI e3=expr? RPAREN s=stmt_before_else
  { node (PS_For1 (e1, e2, e3, s)) $loc }
| FOR LPAREN d=decl e2=expr? SEMI e3=expr? RPAREN s=stmt_before_else
  { node (PS_For2 (d, e2, e3, s)) $loc }

(* 6.8.5
  iteration-statement:
    'while' '(' expression ')' statement
    'do' statement 'while' '(' expression ')' ';'
    'for' '(' expression? ';' expression? ';' expression? ')' statement
    'for' '(' declaration expression? ';' expression? ')' statement
 *)

(* 6.8.6
  jump-statement:
    'goto' identifier ';'
    'continue' ';'
    'break' ';'
    'return' expression? ';'
 *)

(** External definitions **)

(* 6.9
  translation-unit:
    external-declaration
    translation-unit external-declaration

  external-declaration:
    function-definition
    declaration
 *)
translation_unit: list(extdef) EOF {$1}

extdef:
| func_def { Func_Def $1 }
| decl { Decl $1 }

(* 6.9.1
  function-definition:
    declaration-specifiers declarator declaration-list? compound-statement

  declaration-list:
    declaration
    declaration-list declaration
 *)

func_def:
| s=decl_spec d=func_declr pd=list(decl) b=comp_stmt
| s=fake_decl_spec d=func_declr pd=list(decl) b=comp_stmt
| s=incomplete_decl_spec d=func_declr_1not_tyid pd=list(decl) b=comp_stmt
  { mk_func_def (node s $loc(s), d, pd, b) }
| name=IDENT s2=incomplete_decl_spec d=func_declr pd=list(decl) b=comp_stmt
  { let s = { s2 with ds_type_spec = [Typedef_Name name] } in
    mk_func_def (node s ($startpos(name), $endpos(s2)), d, pd, b) }
| name=IDENT d=func_declr_1any_id pd=list(decl) b=comp_stmt
  { let s = { empty_decl_spec with ds_type_spec = [Typedef_Name name] } in
    mk_func_def (node s $loc(name), d, pd, b) }
| d=func_declr_1not_tyid pd=list(decl) b=comp_stmt
  { mk_func_def (node empty_decl_spec ($startpos, $startpos), d, pd, b) }

%inline
func_declr:
  direct_declr func_declr_suffix
  { node ($2 $1) $loc }
(* unnecessary, and causes conflicts *)
(*| LPAREN func_declr RPAREN
  { node (D_Paren $2) $loc }*)

%inline
func_declr_1not_tyid:
  direct_declr_1not_tyid func_declr_suffix
  { node ($2 $1) $loc }

%inline
func_declr_1any_id:
  declr_1any_id func_declr_suffix
  { node ($2 $1) $loc }

func_declr_suffix:
| func_declr_suffix_ansi
| func_declr_suffix_oldstyle
  { $1 }
