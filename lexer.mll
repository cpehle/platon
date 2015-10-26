{

open Token
exception Error

let bounds lo c hi = (lo <= c) && (c <= hi)


}


let ident = ['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let integer = ['0'-'9']+

rule token = parse
	| [' ' '\t' '\r' '\n']  { token lexbuf }
	| "fn"                 { FUN }
	| "let"                 { LET }
	| "in"                  { IN }
	| "forall"              { FORALL }
	| "match"               { MATCH }
    | "module"              { MODULE }
	| '('     { LPAREN }
	| ')'     { RPAREN }
	| '['     { LBRACKET }
	| ']'     { RBRACKET }
	| '{'     { LBRACE }
	| '}'     { RBRACE }
	| '='     { EQUALS }
	| "->"    { ARROW }
	| ','     { COMMA }
	| '.'     { DOT }
	| '-'     { MINUS }
    | '+'     { PLUS }
	| '|'     { PIPE }
	| ':'     { COLON }
	| '"'                          { let buf = Buffer.create 32 in
                                      str buf lexbuf }
	| ident                 { IDENT (Lexing.lexeme lexbuf) }
	| eof     { EOF }
	| _       { raise Error }

and str buf = parse
    _ as ch
    {
      match ch with
          '"' -> STRING (Buffer.contents buf)
        | _ ->
            Buffer.add_char buf ch;
            let c = Char.code ch in
              if bounds 0 c 0x7f
              then str buf lexbuf
              else
                if ((c land 0b1110_0000) == 0b1100_0000)
                then ext_str 1 buf lexbuf
                else
                  if ((c land 0b1111_0000) == 0b1110_0000)
                  then ext_str 2 buf lexbuf
                  else
                    if ((c land 0b1111_1000) == 0b1111_0000)
                    then ext_str 3 buf lexbuf
                    else
                      if ((c land 0b1111_1100) == 0b1111_1000)
                      then ext_str 4 buf lexbuf
                      else
                        if ((c land 0b1111_1110) == 0b1111_1100)
                        then ext_str 5 buf lexbuf
                        else raise Error
    }
and ext_str n buf = parse
    _ as ch
      {
        let c = Char.code ch in
          if ((c land 0b1100_0000) == (0b1000_0000))
          then
            begin
              Buffer.add_char buf ch;
              if n = 1
              then str buf lexbuf
              else ext_str (n-1) buf lexbuf
            end
          else
            raise Error
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
	| EOF -> "<eof>"

}
