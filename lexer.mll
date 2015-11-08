{

open Core.Std
open Token


let bounds lo c hi = (lo <= c) && (c <= hi)

let fail lexbuf s : (Token.t, Parse_error.t * Position.t) Result.t =
    let p = lexbuf.Lexing.lex_start_p in
    let pos =
      (Option.some p.Lexing.pos_fname,
       p.Lexing.pos_lnum ,
       (p.Lexing.pos_cnum) - (p.Lexing.pos_bol))
    in
    Result.Error (Parse_error.LexError s, pos)
  ;;

let bump_line p = { p with
              Lexing.pos_lnum = p.Lexing.pos_lnum + 1;
              Lexing.pos_bol = p.Lexing.pos_cnum }
  ;;

  let newline lexbuf =
    lexbuf.Lexing.lex_curr_p
    <- (bump_line lexbuf.Lexing.lex_curr_p)
  ;;

}

let hexdig = ['0'-'9' 'a'-'f' 'A'-'F']
let decdig = ['0'-'9']
let bin = '0' 'b' ['0' '1' '_']*
let hex = '0' 'x' ['0'-'9' 'a'-'f' 'A'-'F' '_']*
let dec = decdig ['0'-'9' '_']*
let exp = ['e''E']['-''+']? dec
let flo = (dec '.' dec (exp?)) | (dec exp)

let ws = [ ' ' '\t' '\r' ]
let ident = ['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let integer = ['0'-'9']+

rule token = parse
	| ws+ { token lexbuf }
    | '\n'                         { newline lexbuf;
                                       token lexbuf }
    | "--" [^'\n']*                { token lexbuf }
	| "fn"                 { Result.Ok FUN }
	| "let"                 {Result.Ok LET }
	| "in"                  {Result.Ok IN }
	| "forall"              {Result.Ok FORALL }
	| "match"               {Result.Ok MATCH }
    | "module"              {Result.Ok MODULE }
    | "if" {Result.Ok IF}
    | "then" {Result.Ok THEN}
    | "else" {Result.Ok ELSE}
    | "where" {Result.Ok WHERE}
    | "int" {Result.Ok TINT}
    | "bool"{Result.Ok TBOOL}
	| '('     { Result.Ok LPAREN }
	| ')'     { Result.Ok RPAREN }
	| '['     { Result.Ok LBRACKET }
	| ']'     { Result.Ok RBRACKET }
	| '{'     { Result.Ok LBRACE }
	| '}'     { Result.Ok RBRACE }
	| '='     { Result.Ok EQUALS }
	| "->"    { Result.Ok ARROW }
    
	| ','     { Result.Ok COMMA }
	| '.'     { Result.Ok DOT }
	| '-'     { Result.Ok MINUS }
    | '+'     { Result.Ok PLUS }
	| '|'     { Result.Ok PIPE }
	| ':'     { Result.Ok COLON }
    | (bin | hex | dec) as n {Result.Ok (INT (Int64.of_string n))}
    | flo as n {Result.Ok (FLOAT (Float.of_string n))}
	| '"'                          { let buf = Buffer.create 32 in
                                      str buf lexbuf }
	| ident                 { Result.Ok (IDENT (Lexing.lexeme lexbuf)) }
    | _ as c  { let s = Char.escaped c in
                                   fail lexbuf ("Bad character: " ^ s) }
	| eof     { Result.Ok EOF}
	
and comment = parse
    | '\n' {token lexbuf}
    | _ {comment lexbuf}
and str buf = parse
    _ as ch
    {
      match ch with
          '"' -> Result.Ok (STRING (Buffer.contents buf))
        | _ ->
            Buffer.add_char buf ch;
            let c = Char.to_int ch in
              if bounds 0 c 0x7f
              then str buf lexbuf
              else
                if phys_equal (c land 0b1110_0000) 0b1100_0000
                then ext_str 1 buf lexbuf
                else
                  if phys_equal (c land 0b1111_0000) 0b1110_0000
                  then ext_str 2 buf lexbuf
                  else
                    if phys_equal (c land 0b1111_1000) 0b1111_0000
                    then ext_str 3 buf lexbuf
                    else
                      if phys_equal (c land 0b1111_1100) 0b1111_1000
                      then ext_str 4 buf lexbuf
                      else
                        if phys_equal (c land 0b1111_1110) 0b1111_1100
                        then ext_str 5 buf lexbuf
                        else fail lexbuf "Bad initial utf8 byte."
    }
and ext_str n buf = parse
    _ as ch
      {
        let c = Char.to_int ch in
          if phys_equal (c land 0b1100_0000) 0b1000_0000
          then
            begin
              Buffer.add_char buf ch;
              if n = 1
              then str buf lexbuf
              else ext_str (n-1) buf lexbuf
            end
          else
             fail lexbuf "Bad trailing utf8 byte."
      }

{

let string_of_token = function
	| FUN -> "fn"
	| LET -> "let"
	| IN -> "in"
	| FORALL -> "forall"
	| MATCH -> "match"
    | MODULE -> "module"
	| IDENT ident -> ident
	| LPAREN -> "("
	| RPAREN -> ")"
	| LBRACKET -> "["
	| RBRACKET -> "]"
	| LBRACE -> "{"
	| RBRACE -> "}"
	| EQUALS -> "="
	| ARROW -> "->"
	| COMMA -> ","
	| DOT -> "."
	| MINUS -> "-"
    | PLUS -> "+"
	| PIPE -> "|"
	| COLON -> ":"
	| STRING s -> s
    | INT i -> Int64.to_string i
    | FLOAT f -> Float.to_string f
	| EOF -> "<eof>"

}
