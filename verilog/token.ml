open Core.Std

type t = RPAREN
       | LPAREN
       | RPAREN_STAR
       | LPAREN_STAR
       | LBRACKET
       | RBRACKET
       | LBRACE
       | RBRACE
       | LOCALPARAM
       | PARAM
       | TYPE
       | COLON
       | STRUCT
       | INPUT
       | OUTPUT
       | INOUT
       | REG
       | WIRE
       | PLUS
       | ASSIGN
       | IF
       | POUND
       | SEMICOLON
       | BEGIN
       | END
       | AT
       | COMMA
       | LOGIC
       | IDENT of string
       | INT of int
       | POSEDGE
       | NEGEDGE
       | MODULE
       | ENDMODULE
       | CASE
       | ENDCASE
       | EQUALS
       | ERROR of Parse_error.t * Position.t * Position.t
       | UNDEF
       | EOF

let to_string = function
  | RPAREN -> ")"
  | LPAREN -> "("
  | RPAREN_STAR -> "*)"
  | LPAREN_STAR -> "(*"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | COLON -> ":"
  | IF -> "if"
  | INPUT -> "input"
  | OUTPUT -> "output"
  | INOUT -> "inout"
  | LOCALPARAM -> "localparam"
  | PARAM -> "param"
  | TYPE -> "type"
  | PLUS -> "+"
  | IDENT s -> s
  | COMMA -> ","
  | SEMICOLON -> ";"
  | INT i -> Int.to_string i
  | BEGIN -> "begin"
  | END -> "end"
  | AT -> "@"
  | STRUCT -> "struct"
  | POUND -> "#"
  | LOGIC -> "logic"
  | REG -> "reg"
  | WIRE -> "wire"
  | ASSIGN -> "<="
  | POSEDGE -> "posedge"
  | NEGEDGE -> "negedge"
  | MODULE -> "module"
  | ENDMODULE -> "endmodule"
  | CASE -> "case"
  | ENDCASE -> "endcase"
  | EQUALS -> "="
  | EOF -> "EOF"
  | UNDEF -> "UNDEF"
  | ERROR (e,l,l') -> "ERROR"
