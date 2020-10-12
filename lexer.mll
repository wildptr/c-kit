{
open Lexing
open Token

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

(* handles "\n" properly *)
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

exception Invalid_Escape

let is_oct_digit = function
  | '0'..'7' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let digit_value = function
  | '0'..'9' as c -> Char.code c - 48
  | _ -> assert false

let parse_escape_seq s i =
  match s.[!i] with
  | 'n' -> incr i; '\n'
  | 'r' -> incr i; '\r'
  | 't' -> incr i; '\t'
  | 'b' -> incr i; '\b'
  | 'f' -> incr i; '\012'
  | 'v' -> incr i; '\011'
  | 'a' -> incr i; '\007'
  | '0'..'7' as c ->
    (* an octal escape sequence is at most 3 characters long *)
    let lim = min (String.length s) (!i+3) in
    let tmp = ref (digit_value c) in
    incr i;
    while !i < lim && is_oct_digit s.[!i] do
      tmp := !tmp*8 + digit_value (s.[!i]);
      incr i
    done;
    Char.chr !tmp
  | _ -> raise Invalid_Escape

let parse_string s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    match s.[!i] with
    | '\\' ->
      incr i;
      begin
        try
          let c = parse_escape_seq s i in
          Buffer.add_char b c
        with Invalid_Escape ->
          Buffer.add_char b '\\';
          Buffer.add_char b s.[!i];
          incr i
      end
    | c ->
      incr i;
      Buffer.add_char b c
  done;
  Buffer.contents b

exception Error

}

let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'- 'z' 'A'-'Z']

let u_suffix = ['u' 'U']
let l_suffix = "l"|"L"|"ll"|"LL"

let hexprefix = '0' ['x' 'X']

let decnum = decdigit+
let octnum = '0' octdigit+
let hexnum = hexprefix hexdigit+
let num = decnum | octnum | hexnum

let exponent = ['e' 'E']['+' '-']? decdigit+
let fraction  = '.' decdigit+
let decfloat = (decnum? fraction)
       |(decnum exponent)
       |(decnum? fraction exponent)
       | (decnum '.')
              | (decnum '.' exponent)

let hexfraction = hexdigit* '.' hexdigit+ | hexdigit+ '.'
let binexponent = ['p' 'P'] ['+' '-']? decdigit+
let hexfloat = hexprefix hexfraction binexponent
             | hexprefix hexdigit+   binexponent

let floatnum = (decfloat | hexfloat)

let ident = (letter|'_'|'$')(letter|decdigit|'_'|'$')*
let blank = ([' ' '\t' '\012' '\r' '\n'] | "\\\n" )+
let blank_no_newline = [' ' '\t' '\012' '\r']+

let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hexdigit+
let oct_escape = '\\' octdigit octdigit? octdigit?

let string_elem = [^ '"' '\\' '\n'] | ('\\' _)
let char_elem = [^ '\'' '\\' '\n'] | ('\\' _)

rule token s = parse
| "/*"          { add_whitespace s lexbuf;
                  comment s lexbuf;
                  token s lexbuf }
| "//"          { add_whitespace s lexbuf;
                  one_line_comment s lexbuf;
                  token s lexbuf }
| blank         { add_whitespace s lexbuf;
                  token s lexbuf }
| floatnum as s ['f' 'F' 'l' 'L']?
                { FLOAT_LIT s }
| (num as s) ( l_suffix? u_suffix? | u_suffix l_suffix )
                { INT_LIT s }
| '"' (string_elem* as s) '"'
                { STRING_LIT (parse_string s) }
| "'" (char_elem* as s) "'"
                { CHAR_LIT (parse_string s) }
| "..."         { ELLIPSIS }
| "+="          { PLUSEQ }
| "-="          { MINUSEQ }
| "*="          { STAREQ }
| "/="          { SLASHEQ }
| "%="          { PERCENTEQ }
| "|="          { BAREQ }
| "&="          { AMPEQ }
| "^="          { CIRCEQ }
| "<<="         { LTLTEQ }
| ">>="         { GTGTEQ }
| "<<"          { LTLT }
| ">>"          { GTGT }
| "=="          { EQEQ }
| "!="          { BANGEQ }
| "<="          { LTEQ }
| ">="          { GTEQ }
| "="           { EQ }
| "<"           { LT }
| ">"           { GT }
| "++"          { PLUSPLUS }
| "--"          { MINUSMINUS }
| "->"          { ARROW }
| '+'           { PLUS }
| '-'           { MINUS }
| '*'           { STAR }
| '/'           { SLASH }
| '%'           { PERCENT }
| '!'           { BANG }
| "&&"          { AMPAMP }
| "||"          { BARBAR }
| '&'           { AMP }
| '|'           { BAR }
| '^'           { CIRC }
| '?'           { QUEST }
| ':'           { COLON }
| '~'           { TILDE }
| '{'           { LBRACE }
| '}'           { RBRACE }
| '['           { LBRACK }
| ']'           { RBRACK }
| '('           { LPAREN }
| ')'           { RPAREN }
| ';'           { SEMI }
| ','           { COMMA }
| '.'           { DOT }
| ident         { PREIDENT [] }
| eof           { EOF }
| "##"          { HASHHASH }
| '#'           { HASH }
| _             { INVALID }

and comment s = parse
| "*/"          { add_whitespace s lexbuf }
| eof           { () }
| _             { add_whitespace s lexbuf; comment s lexbuf }

(* Does not record whitespace. Used in [skip_to_directive]. *)
and comment' = parse
| "*/"          { () }
| eof           { () }
| '\n'          { next_line lexbuf; comment' lexbuf }
| _             { comment' lexbuf }

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

(* Beware backslash-newline and trailing multi-line comments. *)
and directive buf = parse
| "\\\n"
  { next_line lexbuf; directive buf lexbuf }
| "/*"
  { add_whitespace buf lexbuf; comment buf lexbuf; directive buf lexbuf }
| '\n'
  { next_line lexbuf }
| ([^'\n' '\\' '/']+) as s
  { Buffer.add_string buf s; directive buf lexbuf }
| ('\\' | '/') as c
  { Buffer.add_char buf c; directive buf lexbuf }
| eof {()} (* prevent crash on malformed input *)

and include_file = parse
| blank         { include_file lexbuf }
| '"' [^ '"']* '"'
                { UserInclude }
| '<' [^ '<' '>']* '>'
                { SysInclude }
| _             { raise Error }

and skip_to_directive ok = parse
| '#'           { ok || skip_to_directive false lexbuf }
| blank_no_newline
                { skip_to_directive ok lexbuf }
| '\n'          { next_line lexbuf; skip_to_directive true lexbuf }
| "/*"          { comment' lexbuf; skip_to_directive ok lexbuf }
| _             { skip_to_directive false lexbuf }
| eof           { false }
