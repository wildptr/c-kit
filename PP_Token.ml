type pptoken =
  | EOF
  | Ident
  | IntLit
  | FloatLit
  | CharLit
  | StringLit
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
  | Directive

(*| DEFINE
  | ELSE
  | ENDIF
  | IFDEF
  | IFNDEF
  | INCLUDE
  | UNDEF*)

[@@deriving show { with_path = false }]

type include_file = SysInclude | UserInclude
