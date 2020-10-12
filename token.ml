type token =
  | EOF
  | PREIDENT of string list (* no_expand_list *)
  | INT_LIT of string
  | FLOAT_LIT of string
  | CHAR_LIT of string
  | STRING_LIT of string
  | ELLIPSIS
  | PLUSEQ
  | MINUSEQ
  | STAREQ
  | SLASHEQ
  | PERCENTEQ
  | BAREQ
  | AMPEQ
  | CIRCEQ
  | LTLTEQ
  | GTGTEQ
  | LTLT
  | GTGT
  | EQEQ
  | BANGEQ
  | LTEQ
  | GTEQ
  | EQ
  | LT
  | GT
  | PLUSPLUS
  | MINUSMINUS
  | ARROW
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | BANG
  | AMPAMP
  | BARBAR
  | AMP
  | BAR
  | CIRC
  | QUEST
  | COLON
  | TILDE
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | SEMI
  | COMMA
  | DOT
  | HASH
  | HASHHASH
  | INVALID
  | DIRECTIVE
  | IDENT of string
  | TYPEIDENT of string
  | AUTO
  | BOOL
  | BREAK
  | CASE
  | CHAR
  | CONST
  | CONTINUE
  | DEFAULT
  | DO
  | DOUBLE
  | ELSE
  | ENUM
  | EXTERN
  | FLOAT
  | FOR
  | GOTO
  | IF
  | INLINE
  | INT
  | LONG
  | REGISTER
  | RESTRICT
  | RETURN
  | SHORT
  | SIGNED
  | SIZEOF
  | STATIC
  | STRUCT
  | SWITCH
  | TYPEDEF
  | UNION
  | UNSIGNED
  | VOID
  | VOLATILE
  | WHILE
[@@deriving show { with_path = false }]

type include_file = SysInclude | UserInclude

module M = Map.Make(String)

let keyword_map = [
  "auto"        , AUTO ;
  "_Bool"       , BOOL ;
  "break"       , BREAK ;
  "case"        , CASE ;
  "char"        , CHAR ;
  "const"       , CONST ;
  "continue"    , CONTINUE ;
  "default"     , DEFAULT ;
  "do"          , DO ;
  "double"      , DOUBLE ;
  "else"        , ELSE ;
  "enum"        , ENUM ;
  "extern"      , EXTERN ;
  "float"       , FLOAT ;
  "for"         , FOR ;
  "goto"        , GOTO ;
  "if"          , IF ;
  "inline"      , INLINE ;
  "int"         , INT ;
  "long"        , LONG ;
  "register"    , REGISTER ;
  "restrict"    , RESTRICT ;
  "return"      , RETURN ;
  "short"       , SHORT ;
  "signed"      , SIGNED ;
  "sizeof"      , SIZEOF ;
  "static"      , STATIC ;
  "struct"      , STRUCT ;
  "switch"      , SWITCH ;
  "typedef"     , TYPEDEF ;
  "union"       , UNION ;
  "unsigned"    , UNSIGNED ;
  "void"        , VOID ;
  "volatile"    , VOLATILE ;
  "while"       , WHILE ;
] |> List.to_seq |> M.of_seq

let convert_token kind text =
  match kind with
  | PREIDENT _ ->
    begin match M.find_opt text keyword_map with
      | Some kw -> kw
      | None -> IDENT text
    end
  | _ -> kind
