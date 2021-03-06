{
  open Tokens

  exception Error of string
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { NEWLINE }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '+'
    { PLUS }
| '*'
    { TIMES }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
