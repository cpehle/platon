open Core.Std
open Token
open Lexbuf

let fail {pos_start; pos_end} s : (Token.t, Parse_error.t * Position.t * Position.t) Result.t =
  Result.Error (Parse_error.LexError s, pos_start, pos_end)

let rec token ({stream; pos_end;} as lexbuf) : (Token.t, Parse_error.t * Position.t * Position.t) Result.t =
  let f () = update_position lexbuf in
  let hexdig = [%sedlex.regexp? '0'..'9' |  'a'..'f' | 'A'..'F'] in
  let bin = [%sedlex.regexp? "0b", Plus ('0' | '1' | '_')] in
  let hex = [%sedlex.regexp? "0x", Plus ('0'..'9' | 'a'..'f' | 'A'..'F' | '_')] in
  let decimal = [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_')] in
  let flo = [%sedlex.regexp? ( '0'..'9', Star ('0'..'9' | '_'), '.',  '0'..'9', Star ('0'..'9' | '_'),  Opt  ('e' | 'E'), Opt ('-' | '+'), '0'..'9', Star ('0'..'9' | '_')) | ( '0'..'9', Star ('0'..'9' | '_'),  ('e' | 'E'), Opt ('-' | '+'), '0'..'9', Star ('0'..'9' | '_'))] in
  let ident = [%sedlex.regexp? id_start, Star id_continue] in
  let integer = [%sedlex.regexp? Plus '0'..'9'] in
  match%sedlex stream with
    | Plus white_space -> f (); token lexbuf
    | '\n' ->  f (); new_line lexbuf; token lexbuf
    | "--", Compl '\n' -> f (); comment lexbuf
    | "fn"  -> f (); Result.Ok FUN
    | "let" -> f (); Result.Ok LET
    | "in" -> f (); Result.Ok IN
    | "forall"-> f (); Result.Ok FORALL
    | "match" -> f (); Result.Ok MATCH
    | "module" -> f (); Result.Ok MODULE
    | "if" -> f (); Result.Ok IF
    | "then" -> f (); Result.Ok THEN
    | "else" -> f (); Result.Ok ELSE
    | "where" -> f (); Result.Ok WHERE
    | "int" -> f (); Result.Ok TINT
    | "bool" -> f (); Result.Ok TBOOL
    | flo -> f (); Result.Ok (FLOAT (Float.of_string (lexeme lexbuf)))
    | bin | hex | decimal -> f (); Result.Ok (INT (Int64.of_string (lexeme lexbuf)))
    | '(' -> f (); Result.Ok LPAREN
    | ')' -> f (); Result.Ok RPAREN
    | '[' -> f (); Result.Ok LBRACKET
    | ']' -> f (); Result.Ok RBRACKET
    | '{' -> f (); Result.Ok LBRACE
    | '}' -> f (); Result.Ok RBRACE
    | '=' -> f (); Result.Ok EQUALS
    | "->"-> f (); Result.Ok ARROW
    | ',' -> f (); Result.Ok COMMA
    | '.' -> f (); Result.Ok DOT
    | '-' -> f (); Result.Ok MINUS
    | '+' -> f (); Result.Ok PLUS
    | '|' -> f (); Result.Ok PIPE
    | ':' -> f (); Result.Ok COLON
    | ident -> f (); Result.Ok (IDENT (lexeme lexbuf))
    | eof -> f (); Result.Ok EOF
    | _ -> f (); fail lexbuf "Unexpected character"
and comment ({stream; pos_end;} as lexbuf) = match%sedlex stream with
    | '\n' -> update_position lexbuf; token lexbuf
    | _ -> update_position lexbuf; comment lexbuf
