open Core.Std

module Position = struct
  open Core.Std
  type t = { filename : string  Option.t;
             line : int;
             lineoffset : int;
             columnoffset : int
         }
  let default_position = { filename = None; line = 1; lineoffset = 0; columnoffset = 0}
  let to_string p = let fn = match p.filename with
                    | None -> "<none>"
                    | Some f -> "<" ^ f ^ ">" in
                    sprintf "{ filename = %s;\
                             line = %i;\
                             lineoffset = %i;\
                             columnoffset = %i\
                             }" fn p.line p.lineoffset p.columnoffset
end

module Lexbuf = struct
  type t = {
  stream : Sedlexing.lexbuf ;
  mutable pos_start : Position.t ;
  mutable pos_end : Position.t;
}

let create_lexbuf ?(fn=None) stream =
  let pos_end =
    Position.{
      filename = fn;
      line = 1;
      lineoffset = 0;
      columnoffset = 0;
    }
  in {pos_start = pos_end; pos_end; stream;}

let from_string ?(fn=None) s = create_lexbuf ~fn (Sedlexing.Utf8.from_string s)
let from_channel ?(fn=None) c = create_lexbuf ~fn (Sedlexing.Utf8.from_channel c)


let lexeme { stream } = Sedlexing.Utf8.lexeme stream

let new_line lexbuf =
  let open Sedlexing in
  let lcp = lexbuf.pos_end in
  lexbuf.pos_end <- Position.{ lcp with line = lcp.line+1; lineoffset = lcp.columnoffset}

let update_position ({pos_end; pos_start; stream} as buf) =
  let p_start, p_end = Sedlexing.loc stream in
  buf.pos_start <- {pos_start with  Position.columnoffset = p_start};
  buf.pos_end <- {pos_end with Position.columnoffset = p_end }
end


module Parse_error = struct
  open Core.Std
open Core_extended.Color_print

type t =
    | LexError of string
    | UnexpectedToken of string
    | UnexpectedTokenWithSuggestion of string * string
    | UnexpectedTokenWithExpectation of string * string
    | UnexpectedReserverd
    | UnexpectedIdentifier
    | InternalError
let to_string = function
  | LexError s -> s
  | UnexpectedToken s -> "Unexpected token `" ^ s ^"`"
  | UnexpectedTokenWithSuggestion (token, suggestion) ->
     Printf.sprintf "Unexpected token `%s`. Did you mean `%s`?"
                    token
                    suggestion
  | UnexpectedTokenWithExpectation (token, expectation) ->
     Printf.sprintf "Unexpected token `%s`. Expected %s."
                    token
                    expectation
  | UnexpectedIdentifier -> "Unexpected identifier"
  | UnexpectedReserverd -> "Unexpected reserved"
  | InternalError -> "Internal error"

let mark_string s from until =
  let strlen = String.length s in
  let mark = String.make strlen ' ' in
  if (0 <= from) && (from < until) && (until < strlen) then begin
    for i = from to until do
      String.set mark i '~';
    done;
    end else ();
  if (0 <= from) && (until < strlen) then begin
      String.set mark from '^';
      String.set mark until '^';
    end else ();
  let mark =  color ~color:`Red mark in
  (String.concat ~sep:"\n" [s;mark]) ^ "\n"
end


module Token = struct
  type t = RPAREN
         | LPAREN
         | LBRACKET
         | RBRACKET
         | LBRACE
         | RBRACE
         | COLON
         | STRUCT
         | INPUT
         | OUTPUT
         | INOUT
         | REG
         | WIRE
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
         | EOF

  let to_string = function
    | RPAREN -> ")"
    | LPAREN -> "("
    | LBRACKET -> "["
    | RBRACKET -> "]"
    | LBRACE -> "{"
    | RBRACE -> "}"
    | COLON -> ":"
    | IF -> "if"
    | INPUT -> "input"
    | OUTPUT -> "output"
    | INOUT -> "inout"
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
    | POSEDGE
    | NEGEDGE
    | MODULE -> "module"
    | ENDMODULE -> "endmodule"
    | CASE
    | ENDCASE
    | EQUALS
    | EOF -> "TOK"
    | ERROR (e,l,l') -> "ERROR"
end


module Lexer = struct
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
    | '#' -> f (); POUND
    | '@' -> f (); AT
    | integer -> f(); (INT (Int.of_string (lexeme lexbuf)))
    | ident -> f (); (IDENT (lexeme lexbuf))
    | eof -> f ();  EOF
    | _ -> f (); fail lexbuf "Unexpected character"
  and comment ({stream; pos_end;} as lexbuf) = match%sedlex stream with
    | '\n' -> update_position lexbuf; token lexbuf
    | _ -> update_position lexbuf; comment lexbuf
end

module Ast = struct
  type direction = IN | OUT | INOUT

  type port = {
      name : string;
      dir : direction;
  }

  type vmodule = {
      name : string;
      ports : port list;
      param : (string * int) list
  }
end


module Parser = struct
  open Lexer

  type env = {
      mutable env_peek : Token.t;
      mutable env_lexbuf : Lexbuf.t;
      mutable errors : Parse_error.t list
  }

  let peek (ps:env) : Token.t = ps.env_peek
  let bump (ps:env) : unit = ps.env_peek <- Lexer.token ps.env_lexbuf
  let get_loc (ps:env) = ()

  let token_error ps err = ps.errors <- List.cons err ps.errors

  let parse_interface ps = false
  let parse_package ps = false

  let parse_port ps = []


  let expect ps t = match (peek ps) with
    | t' when t' = t -> bump ps;
    | _ -> (token_error ps (Parse_error.UnexpectedToken "unexpected token"); bump ps;)

  let ident ps = match (peek ps) with
    | Token.IDENT s -> (bump ps; s)
    | _ -> (token_error ps (Parse_error.LexError "unexpected token"); "")

  let parse_module ps =
    let parse_optional_parameters ps =
      match (peek ps) with
      | Token.POUND -> (
        bump ps;
        expect ps (Token.LPAREN);
        expect ps (Token.RPAREN);
        []
      )
      | _ -> []
    in
    let parse_optional_portlist ps = match (peek ps) with
      | Token.LPAREN -> (
        bump ps;
        expect ps (Token.RPAREN);
        []
      )
      | _ -> []
    in
    (*
      ModuleHeader
        ::= GlobalName OptionalParameters OptionalPortList
     *)
    let parse_module_header ps =
      let name = ident ps in
      let params = parse_optional_parameters ps in
      let portlist = parse_optional_portlist ps in
      (name, params, portlist)
    in
    let parse_module_body ps = false in
    bump ps;
    let (name, param, portlist) = parse_module_header ps in
    let module_body = parse_module_body in
    { Ast.name = name; Ast.ports = portlist; Ast.param = param}

  let top_level (ps:env) =
    match (peek ps) with
    | Token.MODULE -> parse_module ps
    (* | Token.PACKAGE -> parse_package ps *)
    (* | Token.INTERFACE -> parse_interface ps *)











end


type var = string

(* verilog types *)
type typ = | Logic of int * int


type ('a, 'b) declaration_form =
  | Register of string * typ
  | Wire of string * typ
 and ('a,'b) module_form = {
  name : string;
  input : (var * typ) list;
  output : (var * typ) list;
  declaration : (('a,'b) declaration_form) list
}

type ('a,'b) statement_form  =
  | Always of 'a * 'b
  | Case of 'a * 'b
  | Instance of 'a * 'b
  | Assign of 'a * 'b

type ('a, 'b) expr_form =
  | Var
  | Binop
  | Concat
and ('a, 'b) elt_form =
  | Bit of int * int

let pp_typ (Logic (low,high)) = "logic " ^ "[" ^ (Int.to_string low) ^ ":" ^ (Int.to_string high) ^ "]"

let pp_module {name; input; output; declaration} =
  let pp_io io (var,typ) = io ^ " " ^ pp_typ typ ^ " " ^ var in
  "module " ^ name ^ "(\n //inputs\n"
  ^ (String.concat ~sep:",\n" (List.map ~f:(pp_io "input") input)) ^ "//outputs\n" ^
    (String.concat ~sep:",\n" (List.map ~f:(pp_io "input") input)) ^ ");\n" ^ "endmodule"

let lex_all code =
  let lexbuf = Lexbuf.from_string code in
  let rec f acc =
    match Lexer.token lexbuf with
    | Token.EOF -> acc
    | tok -> f (tok :: acc)
  in f [] |> List.rev

module PP = struct
  module L = struct
    let module_ = "module"
    let always_comb = "always_comb"
    let always_ff = "always_ff"
    let lparen = "("
    let rparen = ")"
    let dot = "."
    let comma = ","
    let colon = ":"
    let question = "?"
    let semi = ";"
    let else_ = "else"
    let if_ = "if"
    let while_ = "while"
    let case = "case"
    let endcase = "endcase"
    let eq = "="
    let ca = "<="
  end
  module P = Ext_pp
  let semi f = P.string f L.semi

  let expression_desc = ()
  (* let statement_desc = function *)
  (*   | Case (a,b) -> *)
  (*      P.string "asd" L.case; *)
  (*      P.space f; *)
  (*      let cxt = P.paren_group f 1 @@ fun _ ->  expression 0 cxt f e in *)
  (*      P.space f; *)
  (*      P.brace_vgroup f 1 @@ fun _ -> *)
  (*     let cxt = loop cxt f (fun f i -> P.string f (string_of_int i) ) cc in *)
  (*     (match def with *)
  (*      | None -> cxt *)
  (*      | Some def -> *)
  (*        P.group f 1 @@ fun _ -> *)
  (*          P.string f L.default; *)
  (*          P.string f L.colon; *)
  (*          P.newline f; *)
  (*          statement_list  false cxt  f def *)
  (*     ) *)
end

let _ =
  let ex = "module test(input logic[31:0] a, output logic[31:0] b);\
            \ reg [1:0] a;
            \ reg [2:0] b;
            \ always_comb @(posedge clk) begin
            \    a <= b;
            \ end
            endmodule" in
  lex_all ex |> List.map ~f:Token.to_string |> String.concat ~sep:" " |> print_string;
  print_string "\n"

