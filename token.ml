type token =
  | EOF
  | PreIdent of string list (* no_expand_list *)
  | TInt of string * bool * Type_Size.int_size
  | TFloat of string * Type_Size.float_size
  | TChar of string
  | TString of string
  | Ellipsis
  | PlusEq
  | MinusEq
  | StarEq
  | SlashEq
  | PercentEq
  | PipeEq
  | AndEq
  | CircEq
  | LtLtEq
  | GtGtEq
  | LtLt
  | GtGt
  | EqEq
  | BangEq
  | LtEq
  | GtEq
  | Eq
  | Lt
  | Gt
  | PlusPlus
  | MinusMinus
  | Arrow
  | TPlus
  | TMinus
  | Star
  | Slash
  | Percent
  | Bang
  | AndAnd
  | PipePipe
  | TAnd
  | Pipe
  | Circ
  | Quest
  | Colon
  | Tilde
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | LParen
  | RParen
  | Semi
  | Comma
  | Dot
  | Hash
  | HashHash
  | Unknown
  | Directive
  | Ident of string
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
  | INT
  | LONG
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
  "int"         , INT ;
  "long"        , LONG ;
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
  | PreIdent _ ->
    begin match M.find text keyword_map with
      | kw -> kw
      | exception Not_found -> Ident text
    end
  | _ -> kind
