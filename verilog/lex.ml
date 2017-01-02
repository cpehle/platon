open Core.Std
open Token
open Lexbuf

let fail {pos_start; pos_end} s = Token.ERROR (Parse_error.LexError s, pos_start, pos_end)

let rec token ({stream; pos_end;} as lexbuf) : Token.t =
  let f () = update_position lexbuf in
  let hexdig = [%sedlex.regexp? '0'..'9' |  'a'..'f' | 'A'..'F'] in
  let bin = [%sedlex.regexp? "0b", Plus ('0' | '1' | '_')] in
  let hex = [%sedlex.regexp? "0x", Plus ('0'..'9' | 'a'..'f' | 'A'..'F' | '_')] in
  let decimal = [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_')] in
  let ident = [%sedlex.regexp? id_start , Star (id_continue)] in
  let integer = [%sedlex.regexp? Plus '0'..'9'] in
  match%sedlex stream with
  | Plus white_space -> f (); token lexbuf
  | '\n' ->  f (); new_line lexbuf; token lexbuf
  | "module" -> f (); MODULE
  | "endmodule" -> f(); ENDMODULE
  | "case" -> f(); CASE
  | "endcase" -> f(); ENDCASE
  | "logic" -> f (); LOGIC
  | "struct" -> f (); STRUCT
  | "reg" -> f (); REG
  | "wire" -> f (); WIRE
  | "<=" -> f (); ASSIGN
  | "input" -> f (); INPUT
  | "output" -> f (); OUTPUT
  | "inout" -> f (); INOUT
  | "localparam" -> f (); LOCALPARAM
  | "param" -> f (); PARAM
  | "type" -> f (); TYPE
  | "(*" -> f (); LPAREN_STAR
  | "*)" -> f (); RPAREN_STAR
  | '(' -> f (); LPAREN
  | ')' -> f (); RPAREN
  | '[' -> f (); LBRACKET
  | ']' -> f (); RBRACKET
  | '{' -> f (); LBRACE
  | '}' -> f (); RBRACE
  | '=' -> f (); EQUALS
  | ':' -> f (); COLON
  | ';' -> f (); SEMICOLON
  | ',' -> f (); COMMA
  | '+' -> f (); PLUS
  | '#' -> f (); POUND
  | '@' -> f (); AT
  | integer -> f(); (INT (Int.of_string (lexeme lexbuf)))
  | ident -> f (); (IDENT (lexeme lexbuf))
  | eof -> f ();  EOF
  | _ -> f (); fail lexbuf "Unexpected character"
and comment ({stream; pos_end;} as lexbuf) = match%sedlex stream with
  | '\n' -> update_position lexbuf; token lexbuf
  | _ -> update_position lexbuf; comment lexbuf
