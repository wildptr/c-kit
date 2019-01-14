{
open Lexing
open PP_Token

let next_line lexbuf =
  let p = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { p with
      pos_bol = p.pos_cnum;
      pos_lnum = p.pos_lnum + 1 }

let at_bol lexbuf =
  (* Format.printf "lex_start_pos=%d pos_bol=%d@."
    lexbuf.lex_start_pos lexbuf.lex_curr_p.pos_bol; *)
  lexbuf.lex_start_pos = lexbuf.lex_curr_p.pos_bol

module M = Map.Make(String)

type pos = {
  mutable lnum : int;
  mutable bol : int;
  mutable cnum : int
}

let add_whitespace b lexbuf =
  let ws = lexeme lexbuf in
  Buffer.add_string b ws;
  let pos =
    let p = lexbuf.lex_start_p in
    {
      lnum = p.pos_lnum;
      bol  = p.pos_bol;
      cnum = p.pos_cnum
    }
  in
  let it = function
    | '\n' ->
      pos.lnum <- pos.lnum + 1;
      pos.cnum <- pos.cnum + 1;
      pos.bol <- pos.cnum
    | _ ->
      pos.cnum <- pos.cnum + 1
  in
  String.iter it ws;
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with
      pos_lnum = pos.lnum;
      pos_bol = pos.bol }

exception Error

}

let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'- 'z' 'A'-'Z']


let usuffix = ['u' 'U']
let lsuffix = "l"|"L"|"ll"|"LL"
let intsuffix = lsuffix | usuffix | usuffix lsuffix | lsuffix usuffix


let hexprefix = '0' ['x' 'X']

let intnum = decdigit+ intsuffix?
let octnum = '0' octdigit+ intsuffix?
let hexnum = hexprefix hexdigit+ intsuffix?

let exponent = ['e' 'E']['+' '-']? decdigit+
let fraction  = '.' decdigit+
let decfloat = (intnum? fraction)
       |(intnum exponent)
       |(intnum? fraction exponent)
       | (intnum '.')
              | (intnum '.' exponent)

let hexfraction = hexdigit* '.' hexdigit+ | hexdigit+ '.'
let binexponent = ['p' 'P'] ['+' '-']? decdigit+
let hexfloat = hexprefix hexfraction binexponent
             | hexprefix hexdigit+   binexponent

let floatsuffix = ['f' 'F' 'l' 'L']
let floatnum = (decfloat | hexfloat) floatsuffix?

let ident = (letter|'_'|'$')(letter|decdigit|'_'|'$')*
let blank = ([' ' '\t' '\012' '\r' '\n'] | "\\\n" )+

let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hexdigit+
let oct_escape = '\\' octdigit octdigit? octdigit?

let string_elem = [^ '"'] | "\\\""
let string = '"' string_elem* '"'

let char_elem = [^ '\''] | "\\'"
let char = "'" char_elem* "'"

rule token s = parse
| "/*"          { add_whitespace s lexbuf;
                  comment s lexbuf;
                  token s lexbuf }
| "//"          { add_whitespace s lexbuf;
                  one_line_comment s lexbuf;
                  token s lexbuf }
| blank         { add_whitespace s lexbuf;
                  token s lexbuf }
| floatnum      { FloatLit }
| hexnum        { IntLit }
| octnum        { IntLit }
| intnum        { IntLit }
| string        { StringLit }
| char          { CharLit }
| "..."         { Ellipsis }
| "+="          { PlusEq }
| "-="          { MinusEq }
| "*="          { StarEq }
| "/="          { SlashEq }
| "%="          { PercentEq }
| "|="          { PipeEq }
| "&="          { AndEq }
| "^="          { CircEq }
| "<<="         { LtLtEq }
| ">>="         { GtGtEq }
| "<<"          { LtLt }
| ">>"          { GtGt }
| "=="          { EqEq }
| "!="          { BangEq }
| "<="          { LtEq }
| ">="          { GtEq }
| "="           { Eq }
| "<"           { Lt }
| ">"           { Gt }
| "++"          { PlusPlus }
| "--"          { MinusMinus }
| "->"          { Arrow }
| '+'           { Plus }
| '-'           { Minus }
| '*'           { Star }
| '/'           { Slash }
| '%'           { Percent }
| '!'           { Bang }
| "&&"          { AndAnd }
| "||"          { PipePipe }
| '&'           { And }
| '|'           { Pipe }
| '^'           { Circ }
| '?'           { Quest }
| ':'           { Colon }
| '~'           { Tilde }
| '{'           { LBrace }
| '}'           { RBrace }
| '['           { LBrack }
| ']'           { RBrack }
| '('           { LParen }
| ')'           { RParen }
| ';'           { Semi }
| ','           { Comma }
| '.'           { Dot }
| ident         { Ident [] }
| eof           { EOF }
| "##"          { HashHash }
| '#'           { Hash }

and comment s = parse
| "*/"          { add_whitespace s lexbuf }
| eof           { () }
| ( [^'*'] | '*' [^'/'] )+
                { add_whitespace s lexbuf; comment s lexbuf }
| _             { add_whitespace s lexbuf; comment s lexbuf }

and one_line_comment s = parse
| '\n'          { add_whitespace s lexbuf }
| eof           { () }
| [^'\n']+      { add_whitespace s lexbuf; one_line_comment s lexbuf }

(*
and escape_seq = parse
| 'n'           { '\n' }
| 'r'           { '\r' }
| 't'           { '\t' }
| 'b'           { '\b' }
| 'f'           { '\012' }
| 'v'           { '\011' }
| 'a'           { '\007' }
| ['0'-'7']+    { Char.chr (int_of_string ("0o" ^ lexeme lexbuf)) }

and string s = parse
| '"'           { let value = Buffer.contents s.string_contents in
                  Buffer.clear s.string_contents }
| '\\'          { Buffer.add_char s.string_contents (escape_seq lexbuf);
                  string s lexbuf }
| _             { Buffer.add_string s.string_contents (lexeme lexbuf);
                  string s lexbuf }
| eof           { let value = Buffer.contents s.string_contents in
                  Buffer.clear s.string_contents }
*)

and directive = parse
| [^'\n']* '\n' { next_line lexbuf }
| [^'\n']* eof  { () }

and include_file = parse
| blank         { include_file lexbuf }
| '"' [^ '"']* '"'
                { UserInclude }
| '<' [^ '<' '>']* '>'
                { SysInclude }
| _             { raise Error }

and skip_line = parse
| '\n'          { next_line lexbuf }
| [^'\n']+      { skip_line lexbuf }
| eof           { () }

(* invoke only at beginning of line *)
and skip_to_directive = parse
| '#'           { true }
| '\n'          { next_line lexbuf; skip_to_directive lexbuf }
| _             { skip_line lexbuf; skip_to_directive lexbuf }
| eof           { false }
