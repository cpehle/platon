type t = LET
       | RPAREN
       | LPAREN
       | LBRACKET
       | RBRACKET
       | LBRACE
       | RBRACE
       | DOT
       | FORALL
       | MATCH
       | MODULE
       | IN
       | COMMA
       | IDENT of string
       | ARROW
       | EQUALS
       | FUN
       | EOF
       | MINUS
       | PLUS
       | PIPE
       | COLON

let string_of_token t = match t with
                    | LET -> "let"
                    | RPAREN -> ")"
                    | LPAREN -> "("
                    | LBRACKET -> "["
                    | RBRACKET -> "]"
                    | LBRACE -> "{"
                    | RBRACE -> "}"
                    | DOT -> "."
                    | FORALL -> "forall"
                    | MATCH -> "match"
                    | MODULE -> "module"
                    | IN -> "in"
                    | COMMA -> ","
                    | IDENT s -> "identifier"
                    | ARROW -> "->"
                    | EQUALS -> "="
                    | FUN -> "fn"
                    | EOF -> "end of file"
                    | MINUS -> "-"
                    | PLUS -> "+"
                    | PIPE -> "|"
                    | COLON -> ","
