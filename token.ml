open Core.Std

type t = LET
       | RPAREN
       | LPAREN
       | LBRACKET
       | RBRACKET
       | LBRACE
       | RBRACE
       | TINT
       | TBOOL
       | IF
       | THEN
       | ELSE
       | DOT
       | FORALL
       | MATCH
       | MODULE
       | IN
       | WHERE
       | COMMA
       | INT of Int64.t
       | FLOAT of float
       | IDENT of string
       | STRING of string
       | SPACES
       | RIGHTARROW
       | EQUALS
       | FUN
       | EOF
       | MINUS
       | PLUS
       | PIPE
       | COLON

let to_string t = match t with
                    | LET -> "let"
                    | RPAREN -> ")"
                    | LPAREN -> "("
                    | LBRACKET -> "["
                    | RBRACKET -> "]"
                    | LBRACE -> "{"
                    | RBRACE -> "}"
                    | DOT -> "."
                    | IF -> "if"
                    | THEN -> "then"
                    | ELSE -> "else"
                    | TINT -> "int"
                    | TBOOL -> "bool"
                    | STRING s -> "\"" ^ s ^ "\""
                    | FORALL -> "forall"
                    | MATCH -> "match"
                    | MODULE -> "module"
                    | IN -> "in"
                    | FLOAT f -> Float.to_string f
                    | INT i -> Int64.to_string i
                    | COMMA -> ","
                    | IDENT s -> "identifier"
                    | RIGHTARROW -> "→"
                    | EQUALS -> "="
                    | FUN -> "fun"
                    | EOF -> "(end of file)"
                    | MINUS -> "-"
                    | PLUS -> "+"
                    | WHERE -> "where"
                    | PIPE -> "|"
                    | SPACES -> "<spaces>"
                    | COLON -> ","
