{
open Lexing
open Token

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1 }

let at_bol lexbuf =
  (* Format.printf "lex_start_pos=%d pos_bol=%d@."
    lexbuf.lex_start_pos lexbuf.lex_curr_p.pos_bol; *)
  lexbuf.lex_start_pos = lexbuf.lex_curr_p.pos_bol

type state = {
  mutable in_directive : bool;
  mutable has_whitespace : bool;
}

let init_state () =
  { in_directive = false;
    has_whitespace = false }

let directive_map = [
  "define", DEFINE;
  "include", INCLUDE;
  "undef", UNDEF;
] |> BatList.enum |> BatMap.String.of_enum

}

let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'- 'z' 'A'-'Z']


let usuffix = ['u' 'U']
let lsuffix = "l"|"L"|"ll"|"LL"
let intsuffix = lsuffix | usuffix | usuffix lsuffix | lsuffix usuffix
              | usuffix ? "i64"


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
let blank = [' ' '\t' '\012' '\r']+

let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hexdigit+
let oct_escape = '\\' octdigit octdigit? octdigit?

(* Pragmas that are not parsed by CIL.  We lex them as PRAGMA_LINE tokens *)
let no_parse_pragma =
               "warning" | "GCC"
             (* Solaris-style pragmas:  *)
             | "ident" | "section" | "option" | "asm" | "use_section" | "weak"
             | "redefine_extname"
             | "TCS_align"
      | "mark"

rule initial s = parse
| "/*"          { comment lexbuf; initial s lexbuf }
| "//"          { one_line_comment lexbuf; initial s lexbuf }
| blank         { s.has_whitespace <- true; initial s lexbuf }
| '\n'          { next_line lexbuf;
                  s.has_whitespace <- true;
                  if s.in_directive then
                    (s.in_directive <- false; EOL)
                  else initial s lexbuf }
| floatnum      { FloatLit (Lexing.lexeme lexbuf) }
| hexnum        { IntLit (Lexing.lexeme lexbuf) }
| octnum        { IntLit (Lexing.lexeme lexbuf) }
| intnum        { IntLit (Lexing.lexeme lexbuf) }
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
| "sizeof"      { SIZEOF }
| ident         { Ident (Lexing.lexeme lexbuf) }
| eof           { EOF }
| "##"          { HashHash }
| '#'           { if at_bol lexbuf then
                    (s.in_directive <- true; directive lexbuf)
                  else Hash }

and comment = parse
| "*/"          { () }
| _             { comment lexbuf }

and one_line_comment = parse
| '\n' | eof    { () }
| _             { one_line_comment lexbuf }

and directive = parse
| blank         { directive lexbuf }
| ident as s    { BatMap.String.find s directive_map }
