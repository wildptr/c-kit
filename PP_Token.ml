type pptoken =
  | EOF
  | Ident of string
  | IntLit of string
  | FloatLit of string
  | CharLit of string
  | StringLit of string
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
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Bang
  | AndAnd
  | PipePipe
  | And
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
  | EOL

  | DEFINE
  | ELSE
  | ENDIF
  | IFDEF
  | IFNDEF
  | INCLUDE
  | UNDEF

(*| BOOL
  | CHAR
  | CONST
  | DOUBLE
  | ELSE
  | EXTERN
  | FLOAT
  | INT
  | LONG
  | SHORT
  | SIGNED
  | SIZEOF
  | STATIC
  | TYPEDEF
  | UNSIGNED
  | VOID
  | VOLATILE*)

[@@deriving show]
