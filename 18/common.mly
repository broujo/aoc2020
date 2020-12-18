%start <int list> main

%%

main:
| stmt = statement EOF { [stmt] }
| stmt = statement m = main { stmt :: m}

statement:
| e = expr NEWLINE { e }
| e = expr { e }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
