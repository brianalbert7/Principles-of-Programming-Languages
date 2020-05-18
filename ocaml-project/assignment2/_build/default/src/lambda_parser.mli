type token =
  | EOF
  | LAMBDA
  | DOT
  | L_PAREN
  | R_PAREN
  | COMMA
  | IDENT of (string)

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.expr
