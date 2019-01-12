open PP_Token

module P = Parser
module M = Map.Make(String)

let keyword_map = [
  "break"       , P.BREAK ;
  "case"        , P.CASE ;
  "char"        , P.CHAR ;
  "const"       , P.CONST ;
  "continue"    , P.CONTINUE ;
  "default"     , P.DEFAULT ;
  "do"          , P.DO ;
  "double"      , P.DOUBLE ;
  "else"        , P.ELSE ;
  "extern"      , P.EXTERN ;
  "float"       , P.FLOAT ;
  "for"         , P.FOR ;
  "goto"        , P.GOTO ;
  "if"          , P.IF ;
  "int"         , P.INT ;
  "long"        , P.LONG ;
  "return"      , P.RETURN ;
  "short"       , P.SHORT ;
  "signed"      , P.SIGNED ;
  "sizeof"      , P.SIZEOF ;
  "static"      , P.STATIC ;
  "switch"      , P.SWITCH ;
  "typedef"     , P.TYPEDEF ;
  "unsigned"    , P.UNSIGNED ;
  "void"        , P.VOID ;
  "volatile"    , P.VOLATILE ;
  "while"       , P.WHILE ;
] |> List.to_seq |> M.of_seq

let convert_token = function
  | EOF ->
    P.EOF
  | Ident s ->
    begin match M.find s keyword_map with
      | kw -> kw
      | exception Not_found -> P.Ident s
    end
  | IntLit s ->
    P.IntLit s
  | FloatLit s ->
    P.FloatLit s
  | CharLit s ->
    P.CharLit s
  | StringLit s ->
    P.StringLit s
  | Ellipsis ->
    P.Ellipsis
  | PlusEq ->
    P.PlusEq
  | MinusEq ->
    P.MinusEq
  | StarEq ->
    P.StarEq
  | SlashEq ->
    P.SlashEq
  | PercentEq ->
    P.PercentEq
  | PipeEq ->
    P.PipeEq
  | AndEq ->
    P.AndEq
  | CircEq ->
    P.CircEq
  | LtLtEq ->
    P.LtLtEq
  | GtGtEq ->
    P.GtGtEq
  | LtLt ->
    P.LtLt
  | GtGt ->
    P.GtGt
  | EqEq ->
    P.EqEq
  | BangEq ->
    P.BangEq
  | LtEq ->
    P.LtEq
  | GtEq ->
    P.GtEq
  | Eq ->
    P.Eq
  | Lt ->
    P.Lt
  | Gt ->
    P.Gt
  | PlusPlus ->
    P.PlusPlus
  | MinusMinus ->
    P.MinusMinus
  | Arrow ->
    P.Arrow
  | Plus ->
    P.Plus
  | Minus ->
    P.Minus
  | Star ->
    P.Star
  | Slash ->
    P.Slash
  | Percent ->
    P.Percent
  | Bang ->
    P.Bang
  | AndAnd ->
    P.AndAnd
  | PipePipe ->
    P.PipePipe
  | And ->
    P.And
  | Pipe ->
    P.Pipe
  | Circ ->
    P.Circ
  | Quest ->
    P.Quest
  | Colon ->
    P.Colon
  | Tilde ->
    P.Tilde
  | LBrace ->
    P.LBrace
  | RBrace ->
    P.RBrace
  | LBrack ->
    P.LBrack
  | RBrack ->
    P.RBrack
  | LParen ->
    P.LParen
  | RParen ->
    P.RParen
  | Semi ->
    P.Semi
  | Comma ->
    P.Comma
  | Dot ->
    P.Dot
  | Hash ->
    P.Hash

  | EOL
  | HashHash
  | DEFINE
  | ELSE
  | ENDIF
  | IFDEF
  | IFNDEF
  | INCLUDE
  | UNDEF ->
    failwith "convert_token"
