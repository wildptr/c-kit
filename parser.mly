%{
open Syntax
open Util

let node body (pos0, pos1) =
  { body; loc = Lexing.(pos0.pos_cnum, pos1.pos_cnum) }

let empty_decl_spec =
  { ds_stor=[]; ds_type_spec=[]; ds_type_qual=[]; ds_func_spec=[] }

let mk_func_def (fd_decl_spec, fd_declarator, fd_oldstyle_param_decl, fd_body) =
  { fd_decl_spec; fd_declarator; fd_oldstyle_param_decl; fd_body }

let contains_typedef declspec =
  List.mem Typedef declspec.ds_stor

let register_name_opt kind name_opt =
  may (Ctx.register_name kind) name_opt

let handle_decl (ds_node, init_declr_list) =
  let kind =
    if contains_typedef ds_node.body then Context.Type
    else Context.Variable
  in
  List.iter (fun (declr, _) -> register_name_opt kind (declarator_name declr.body))
    init_declr_list

let handle_param_decl (_, declr) =
  register_name_opt Context.Variable (declarator_name declr.body)

let rec handle_fd_declarator dnode =
  match dnode.body with
  | D_Func ({ body = D_Base name_opt; _ }, param_decl_list, _) ->
    register_name_opt Context.Variable name_opt;
    Ctx.enter_scope ();
    List.iter handle_param_decl param_decl_list
  | D_Ptr (dnode', _) | D_Array (dnode', _) | D_Paren dnode'
  | D_Func (dnode', _, _) | D_Old_Func (dnode', _) ->
    handle_fd_declarator dnode'
  | D_Base name_opt ->
    register_name_opt Context.Variable name_opt;
    Ctx.enter_scope ()

%}

%parameter <Ctx : Context.S>

%token EOF

%token <string list> (* no_expand_list *) PREIDENT
%token DIRECTIVE
%token HASHHASH
%token HASH
%token INVALID

%token <string> IDENT
%token <string> TYPEIDENT
%token <string> INT_LIT
%token <string> FLOAT_LIT
%token <string> STRING_LIT
%token <string> CHAR_LIT

%token ELLIPSIS     "..."
%token PLUSEQ       "+="
%token MINUSEQ      "-="
%token STAREQ       "*="
%token SLASHEQ      "/="
%token PERCENTEQ    "%="
%token BAREQ        "|="
%token AMPEQ        "&="
%token CIRCEQ       "^="
%token LTLTEQ       "<<="
%token GTGTEQ       ">>="
%token LTLT         "<<"
%token GTGT         ">>"
%token EQEQ         "=="
%token BANGEQ       "!="
%token LTEQ         "<="
%token GTEQ         ">="
%token EQ           "="
%token LT           "<"
%token GT           ">"
%token PLUSPLUS     "++"
%token MINUSMINUS   "--"
%token ARROW        "->"
%token PLUS         "+"
%token MINUS        "-"
%token STAR         "*"
%token SLASH        "/"
%token PERCENT      "%"
%token BANG         "!"
%token AMPAMP       "&&"
%token BARBAR       "||"
%token AMP          "&"
%token BAR          "|"
%token CIRC         "^"
%token QUEST        "?"
%token COLON        ":"
%token TILDE        "~"
%token LBRACE       "{"
%token RBRACE       "}"
%token LBRACK       "["
%token RBRACK       "]"
%token LPAREN       "("
%token RPAREN       ")"
%token SEMI         ";"
%token COMMA        ","
%token DOT          "."

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

%left "||"
%left "&&"
%left "|"
%left "^"
%left "&"
%left "==" "!="
%left "<" ">" "<=" ">="
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"

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
| "(" expr_noid ")"
  { node (PE_Paren $2) $loc }
| "(" IDENT ")"
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
| postfix_expr "[" expr "]"
  { node (PE_Index ($1, $3)) $loc }
| postfix_expr "(" separated_list(",", assign_expr) ")"
  { node (PE_Call ($1, Array.of_list $3)) $loc }
| postfix_expr "." ident_loc
  { node (PE_Dot ($1, $3)) $loc }
| postfix_expr "->" ident_loc
  { node (PE_Arrow ($1, $3)) $loc }
| postfix_expr "++"
  { node (PE_IncDec (POST_INC, $1)) $loc }
| postfix_expr "--"
  { node (PE_IncDec (POST_DEC, $1)) $loc }

postfix_expr:
| postfix_expr_noid {$1}
| IDENT { node (PE_Ident $1) $loc }

postfix_expr_cast:
| prim_expr_cast {$1}
| postfix_expr_cast "[" expr "]"
  { node (PE_Index ($1, $3)) $loc }
| postfix_expr_cast "." ident_loc
  { node (PE_Dot ($1, $3)) $loc }
| postfix_expr_cast "->" ident_loc
  { node (PE_Arrow ($1, $3)) $loc }
| postfix_expr_cast "++"
  { node (PE_IncDec (POST_INC, $1)) $loc }
| postfix_expr_cast "--"
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
| SIZEOF "(" typ ")"
  { node (PE_SizeofT $3) $loc }
| "~" unary_expr
  { node (PE_Unary (NOT, $2)) $loc }
| "!" unary_expr
  { node (PE_Unary (LOGIC_NOT, $2)) $loc }

unary_expr_cast:
| postfix_expr_cast {$1}
| unary_expr_common {$1}

unary_expr_noid:
| postfix_expr_noid {$1}
| unary_expr_common {$1}
| "++" unary_expr
  { node (PE_IncDec (PRE_INC, $2)) $loc }
| "--" unary_expr
  { node (PE_IncDec (PRE_DEC, $2)) $loc }
| "&" unary_expr
  { node (PE_Addr $2) $loc }
| "*" unary_expr
  { node (PE_Deref $2) $loc }
| "+" unary_expr
  { node (PE_Unary (OPLUS, $2)) $loc }
| "-" unary_expr
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
| "(" typ ")" cast_expr
  { node (PE_Cast ($2, $4)) $loc }
| "(" IDENT ")" unary_expr_cast
  { let typ = node (([Typedef_Name $2], []), None) $loc($2) in
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
| bin_expr "*"  bin_expr { node (PE_Binary (MUL,        $1, $3)) $loc }
| bin_expr "/"  bin_expr { node (PE_Binary (DIV,        $1, $3)) $loc }
| bin_expr "%"  bin_expr { node (PE_Binary (MOD,        $1, $3)) $loc }
| bin_expr "+"  bin_expr { node (PE_Binary (ADD,        $1, $3)) $loc }
| bin_expr "-"  bin_expr { node (PE_Binary (SUB,        $1, $3)) $loc }
| bin_expr "<<" bin_expr { node (PE_Binary (LSHIFT,     $1, $3)) $loc }
| bin_expr ">>" bin_expr { node (PE_Binary (RSHIFT,     $1, $3)) $loc }
| bin_expr "<"  bin_expr { node (PE_Binary (LESS,       $1, $3)) $loc }
| bin_expr ">"  bin_expr { node (PE_Binary (GREATER,    $1, $3)) $loc }
| bin_expr "<=" bin_expr { node (PE_Binary (LESS_EQ,    $1, $3)) $loc }
| bin_expr ">=" bin_expr { node (PE_Binary (GREATER_EQ, $1, $3)) $loc }
| bin_expr "==" bin_expr { node (PE_Binary (EQUAL,      $1, $3)) $loc }
| bin_expr "!=" bin_expr { node (PE_Binary (NOT_EQUAL,  $1, $3)) $loc }
| bin_expr "&"  bin_expr { node (PE_Binary (AND,        $1, $3)) $loc }
| bin_expr "^"  bin_expr { node (PE_Binary (XOR,        $1, $3)) $loc }
| bin_expr "|"  bin_expr { node (PE_Binary (OR,         $1, $3)) $loc }
| bin_expr "&&" bin_expr { node (PE_Binary (LOGIC_AND,  $1, $3)) $loc }
| bin_expr "||" bin_expr { node (PE_Binary (LOGIC_OR,   $1, $3)) $loc }

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
| bin_expr "?" expr ":" cond_expr
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
| "*="  { MUL }
| "/="  { DIV }
| "%="  { MOD }
| "+="  { ADD }
| "-="  { SUB }
| "<<=" { LSHIFT }
| ">>=" { RSHIFT }
| "&="  { AND }
| "^="  { XOR }
| "|="  { OR }

(* 6.5.17
  expression:
    assignment-expression
    expression ',' assignment-expression
 *)
expr_noid:
| assign_expr_noid {$1}
| expr "," assign_expr
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
decl_body:
| s=decl_spec hd=init_declr tl=list(preceded(",", init_declr))
| s=fake_decl_spec_safe hd=init_declr tl=list(preceded(",", init_declr))
| s=fake_decl_spec_id_first hd=init_declr tl=list(preceded(",", init_declr))
| s=fake_decl_spec_id_last_unsafe hd=init_declr_(declr_1_any_id) tl=list(preceded(",", init_declr))
| s=incomplete_decl_spec hd=init_declr_(declr_1not_tyid) tl=list(preceded(",", init_declr))
  { let d = (node s $loc(s), hd::tl) in
    handle_decl d; d }
| s=decl_spec
| s=fake_decl_spec_safe
| s=fake_decl_spec_id_first
| s=incomplete_decl_spec
  { let d = (node s $loc(s), []) in
    handle_decl d; d }

decl: decl_body ";" {$1}

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
fake_decl_spec_safe:
  s1=incomplete_decl_spec name=IDENT s2=incomplete_decl_spec
  { Ctx.assume_typename name;
    { ds_stor = s1.ds_stor @ s2.ds_stor;
      ds_type_spec = [Typedef_Name name];
      ds_type_qual = s1.ds_type_qual @ s2.ds_type_qual;
      ds_func_spec = s1.ds_func_spec @ s2.ds_func_spec } }

(*
%inline
fake_decl_spec_id_last:
  s1=incomplete_decl_spec name=IDENT
  { Ctx.assume_typename name;
    { s1 with ds_type_spec = [Typedef_Name name] }}
*)

%inline
fake_decl_spec_id_last_unsafe:
  s1=incomplete_decl_spec_opt name=IDENT
  { Ctx.assume_typename name;
    { s1 with ds_type_spec = [Typedef_Name name] }}

%inline
fake_decl_spec_id_first:
  name=IDENT s2=incomplete_decl_spec
  { Ctx.assume_typename name;
    { s2 with ds_type_spec = [Typedef_Name name] } }

init_declr_(declr_):
| declr_
  { ($1, None) }
| declr_ EQ init
  { ($1, Some $3) }

%inline init_declr: init_declr_(declr) {$1}

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
| VOID      { Void }
| CHAR      { Char }
| SHORT     { Short }
| INT       { Int }
| LONG      { Long }
| FLOAT     { Float }
| DOUBLE    { Double }
| SIGNED    { Signed }
| UNSIGNED  { Unsigned }
| BOOL      { Bool }
| su_spec   { $1 }
| enum_spec { Enum $1 }

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
su_spec:
| su=su name=ident_loc? "{" decl=nonempty_list(struct_decl) "}"
  { su (SU1 (name, decl)) }
| su=su name=ident_loc
  { su (SU0 name) }

su:
| STRUCT { fun spec -> Struct spec }
| UNION  { fun spec -> Union  spec }

struct_decl:
| s=decl_spec hd=struct_declr tl=list(preceded(",", struct_declr)) ";"
| s=fake_decl_spec_safe hd=struct_declr tl=list(preceded(",", struct_declr)) ";"
| s=fake_decl_spec_id_first hd=struct_declr tl=list(preceded(",", struct_declr)) ";"
| s=fake_decl_spec_id_last_unsafe hd=struct_declr_after_id tl=list(preceded(",", struct_declr)) ";"
| s=incomplete_decl_spec hd=struct_declr_(declr_1not_tyid) tl=list(preceded(",", struct_declr)) ";"
  { (node s $loc(s), hd::tl) }
(* EXTENSION: a declaration may contain no declarators, useful for anonymous
   structs and unions *)
| s=decl_spec ";"
| s=fake_decl_spec_safe ";"
| s=fake_decl_spec_id_first ";"
| s=incomplete_decl_spec ";" (* this is likely to be an error *)
  { (node s $loc(s), []) }

struct_declr_(declr_):
| declr_
  {(Some $1, None)}
| declr_? ":" const_expr
  {($1, Some $3)}

%inline struct_declr: struct_declr_(declr) {$1}

struct_declr_after_id:
| declr_1_any_id
  {(Some $1, None)}
| declr_1_any_id ":" const_expr (* note that the declarator is mandatory *)
  {(Some $1, Some $3)}

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
| name=IDENT q=nonempty_list(type_qual)
  { Ctx.assume_typename name;
    ([Typedef_Name name], q) }
| q1=nonempty_list(type_qual) name=IDENT q2=list(type_qual)
  { Ctx.assume_typename name;
    ([Typedef_Name name], q1@q2) }

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
enum_spec:
| ENUM name=ident_loc? "{" elist=enumerator_list "}"
  { Enum1 (name, elist) }
| ENUM name=ident_loc
  { Enum0 name }

enumerator_list:
| enumerator
| enumerator "," {[$1]}
| enumerator "," enumerator_list {$1::$3}

enumerator:
| ident_loc
  {($1, None)}
| ident_loc "=" const_expr
  {($1, Some $3)}

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

(*
declr_1not_tyid: (* first terminal is not TYPEIDENT *)
| direct_declr_1not_tyid {$1}
| ptr direct_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }
*)

%inline (* avoid conflicts *)
declr_1not_tyid:
| id_declr
| declr_1_id_suffixed
| declr_1not_any_id
  {$1}

declr_1_any_id:
| IDENT | TYPEIDENT
  { node (D_Base (Some $1)) $loc }
| declr_1_any_id declr_suffix
  { node ($2 $1) $loc }

(* For disambiguation in old-style function declarations.
   Begins with IDENT and contains at least one declarator suffix. *)
declr_1_id_suffixed:
| IDENT declr_suffix
  { let d = node (D_Base (Some $1)) $loc($1) in
    node ($2 d) $loc }
| declr_1_id_suffixed declr_suffix
  { node ($2 $1) $loc }

declr_1not_any_id:
| direct_declr_1not_any_id {$1}
| ptr direct_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }

direct_declr_1not_any_id:
| "(" declr ")"
  { node (D_Paren $2) $loc }
| direct_declr_1not_any_id declr_suffix
  { node ($2 $1) $loc }

func_declr_suffix_ansi:
| "(" param_type_list ")"
  { let (decls, vararg) = $2 in
    fun d -> D_Func (d, decls, vararg) }
| "(" ")"
  { fun d -> D_Old_Func (d, []) }

func_declr_suffix_oldstyle:
  "(" hd=IDENT tl=list(preceded(",", any_ident)) ")"
  { fun d -> D_Old_Func (d, hd::tl) }

abs_declr_suffix:
| "[" q=list(type_qual) e=assign_expr? "]"
  { fun d -> D_Array (d, {ad_size_opt=e; ad_type_qual=q; ad_static_flag=false}) }
| "[" STATIC q=list(type_qual) e=assign_expr "]"
| "[" q=nonempty_list(type_qual) STATIC e=assign_expr "]"
  { fun d -> D_Array (d, {ad_size_opt=Some e; ad_type_qual=q; ad_static_flag=true}) }
| "[" q=list(type_qual) "*" "]"
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
  { node (D_Base (Some $1)) $loc }
| "(" declr ")"
  { node (D_Paren $2) $loc }
| direct_declr declr_suffix
  { node ($2 $1) $loc }

(*
direct_declr_1not_tyid:
| IDENT
  { node (D_Base (Some $1)) $loc }
| "(" declr ")"
  { node (D_Paren $2) $loc }
| direct_declr_1not_tyid declr_suffix
  { node ($2 $1) $loc }
*)

param_type_list:
| param_decl
  { ([$1], false) }
| param_decl "," "..."
  { ([$1], true) }
| param_decl "," param_type_list
  { let decls, vararg = $3 in
    ($1::decls, vararg) }

(*
  int f(int(T()))
  => int f(int _(T _()))        (T is typedef-name)
  => int f(int T())             (T is not typedef-name)
 *)
param_decl:
| decl_spec opt_declr
| incomplete_decl_spec opt_declr_1not_any_id (* missing type specifier *)
  { (node $1 $loc($1), $2) }
| s1=incomplete_decl_spec_opt name=IDENT s2=incomplete_decl_spec_opt d=opt_declr
  { Ctx.assume_typename name;
    let s =
      { ds_stor = s1.ds_stor @ s2.ds_stor;
        ds_type_spec = [Typedef_Name name];
        ds_type_qual = s1.ds_type_qual @ s2.ds_type_qual;
        ds_func_spec = s1.ds_func_spec @ s2.ds_func_spec }
    in (node s ($startpos(s1), $endpos(s2)), d) }
| decl_spec
  { (node $1 $loc($1), node (D_Base None) ($endpos, $endpos)) }

ptr:
| "*" q=list(type_qual)
  { [(q, $startpos)] }
| "*" q=list(type_qual) p=ptr
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
| sq=spec_qual_list_aug d=abs_declr?
  { node (sq, d) $loc }

opt_declr: direct_opt_declr | indirect_opt_declr {$1}

opt_declr_1not_any_id: direct_opt_declr_1not_any_id | indirect_opt_declr {$1}

indirect_opt_declr:
| ptr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 (node (D_Base None) ($endpos, $endpos)) }
| ptr direct_opt_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }

(*
    int f(int(T));
 *)

direct_opt_declr:
| IDENT | TYPEIDENT
  { node (D_Base (Some $1)) $loc }
| "(" enclosed_opt_declr ")"
  { node (D_Paren $2) $loc }
| abs_declr_suffix
  { node ($1 (node (D_Base None) ($endpos, $endpos))) $loc }
| direct_opt_declr abs_declr_suffix
  { node ($2 $1) $loc }

direct_opt_declr_1not_any_id:
| "(" enclosed_opt_declr ")"
  { node (D_Paren $2) $loc }
| abs_declr_suffix
  { node ($1 (node (D_Base None) ($endpos, $endpos))) $loc }
| direct_opt_declr_1not_any_id abs_declr_suffix
  { node ($2 $1) $loc }

enclosed_opt_declr:
| indirect_opt_declr
| enclosed_opt_declr_aux
  { $1 }
| IDENT
  { node (D_Base (Some $1)) $loc }

enclosed_opt_declr_aux:
| "(" d=enclosed_opt_declr ")"
  { node (D_Paren d) $loc }
| d=abs_declr_suffix
  { node (d (node (D_Base None) ($endpos, $endpos))) $loc }
| enclosed_opt_declr_aux abs_declr_suffix
  { node ($2 $1) $loc }

abs_declr:
| direct_abs_declr {$1}
| ptr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 (node (D_Base None) ($endpos, $endpos)) }
| ptr direct_abs_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }

direct_abs_declr:
| "(" abs_declr ")"
  { node (D_Paren $2) $loc }
| direct_abs_declr abs_declr_suffix
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
| "{" separated_nonempty_list(",", init_list_item) "}"
| "{" nonempty_list(terminated(init_list_item, ",")) "}"
  { Init_List (node $2 $loc) }

init_list_item:
| init
  { (node [] ($startpos, $startpos), $1) }
| designation init
  { ($1, $2) }

designation: nonempty_list(designator) EQ { node $1 $loc }

designator:
| "[" const_expr "]"
  { Desig_Index $2 }
| "." ident_loc
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
comp_stmt: scope_start "{" body=comp_stmt_body "}" {body}
fd_comp_stmt: "{" body=comp_stmt_body "}" {body}

scope_start: { Ctx.enter_scope () }

comp_stmt_body: list(block_item)
  { Ctx.leave_scope (); node (PS_Block $1) $loc }

block_item:
| decl { Item_Decl $1 }
| stmt { Item_Stmt $1 }

(* 6.8.3
  expression-stmt:
    expression? ';'
 *)
semicolon_terminated_stmt:
| expr ";"
  { node (PS_Expr $1) $loc }
| ";"
  { node PS_Null $loc }
| DO s=stmt WHILE "(" e=expr ")" ";"
  { node (PS_Do_While (s, e)) $loc }
| GOTO ident_loc ";"
  { node (PS_Goto $2) $loc }
| CONTINUE ";"
  { node PS_Continue $loc }
| BREAK ";"
  { node PS_Break $loc }
| RETURN expr? ";"
  { node (PS_Return $2) $loc }

stmt_terminated_stmt:
(* labeled *)
| name=any_ident ":" s=stmt
  { node (PS_Labeled (s, Label name)) $loc }
| CASE e=const_expr ":" s=stmt
  { node (PS_Labeled (s, Label_Case e)) $loc }
| DEFAULT ":" s=stmt
  { node (PS_Labeled (s, Label_Default)) $loc }
(* selection *)
| IF "(" e=expr ")" s=stmt
  { node (PS_If (e, s)) $loc }
| IF "(" e=expr ")" s1=stmt_before_else ELSE s2=stmt
  { node (PS_Ifelse (e, s1, s2)) $loc }
| SWITCH "(" e=expr ")" s=stmt
  { node (PS_Switch (e, s)) $loc }
(* iteration *)
| WHILE "(" e=expr ")" s=stmt
  { node (PS_While (e, s)) $loc }
| FOR "(" e1=expr? ";" e2=expr? ";" e3=expr? ")" s=stmt
  { node (PS_For1 (e1, e2, e3, s)) $loc }
| FOR "(" d=decl e2=expr? ";" e3=expr? ")" s=stmt
  { node (PS_For2 (d, e2, e3, s)) $loc }

(* 6.8.4
  selection-statement:
    'if' '(' expression ')' statement
    'if' '(' expression ')' statement 'else' statement
    'switch' '(' expression ')' statement
 *)

stmt_terminated_stmt_before_else:
(* labeled *)
| name=any_ident ":" s=stmt_before_else
  { node (PS_Labeled (s, Label name)) $loc }
| CASE e=const_expr ":" s=stmt_before_else
  { node (PS_Labeled (s, Label_Case e)) $loc }
| DEFAULT ":" s=stmt_before_else
  { node (PS_Labeled (s, Label_Default)) $loc }
(* selection *)
| IF "(" e=expr ")" s1=stmt_before_else ELSE s2=stmt_before_else
  { node (PS_Ifelse (e, s1, s2)) $loc }
| SWITCH "(" e=expr ")" s=stmt_before_else
  { node (PS_Switch (e, s)) $loc }
(* iteration *)
| WHILE "(" e=expr ")" s=stmt_before_else
  { node (PS_While (e, s)) $loc }
| FOR "(" e1=expr? ";" e2=expr? ";" e3=expr? ")" s=stmt_before_else
  { node (PS_For1 (e1, e2, e3, s)) $loc }
| FOR "(" d=decl e2=expr? ";" e3=expr? ")" s=stmt_before_else
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
| s=decl_spec d=fd_(declr) pd=list(decl) b=fd_comp_stmt
| s=fake_decl_spec_safe d=fd_(declr) pd=list(decl) b=fd_comp_stmt
| s=fake_decl_spec_id_first d=fd_(declr) pd=list(decl) b=fd_comp_stmt
| s=fake_decl_spec_id_last_unsafe d=fd_(declr_1_any_id) pd=list(decl) b=fd_comp_stmt
| s=incomplete_decl_spec d=fd_(declr_1_id_suffixed) pd=list(decl) b=fd_comp_stmt
| s=incomplete_decl_spec d=fd_(declr_1not_any_id) pd=list(decl) b=fd_comp_stmt
  { mk_func_def (node s $loc(s), d, pd, b) }
| d=fd_(func_declr_1not_tyid) pd=list(decl) b=fd_comp_stmt
  { mk_func_def (node empty_decl_spec ($startpos, $startpos), d, pd, b) }

fd_(declr_): declr_
  { handle_fd_declarator $1; $1 }

func_declr:
| direct_func_declr {$1}
| ptr direct_func_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }

direct_func_declr:
| any_id_declr func_declr_suffix
| direct_func_declr declr_suffix
  { node ($2 $1) $loc }
| "(" func_declr ")"
  { node (D_Paren $2) $loc }

func_declr_1not_tyid:
| direct_func_declr_1not_tyid {$1}
| ptr direct_func_declr
  { List.fold_right (fun (q, pos) acc -> node (D_Ptr (acc, q)) (pos, $endpos)) $1 $2 }

direct_func_declr_1not_tyid:
| id_declr func_declr_suffix
| direct_func_declr_1not_tyid declr_suffix
  { node ($2 $1) $loc }
| "(" func_declr ")"
  { node (D_Paren $2) $loc }

func_declr_suffix:
| func_declr_suffix_ansi
| func_declr_suffix_oldstyle
  { $1 }

%inline
any_id_declr:
  IDENT | TYPEIDENT
  { node (D_Base (Some $1)) $loc }

%inline
id_declr: IDENT
  { node (D_Base (Some $1)) $loc }
