open Core.Std
open Lex

type env = {
    mutable env_peek : Token.t;
    mutable env_lexbuf : Lexbuf.t;
    mutable errors : Parse_error.t list
  }


let peek (ps:env) : Token.t = ps.env_peek
let bump (ps:env) : unit = ps.env_peek <- Lex.token ps.env_lexbuf

let skip_until (ps:env) (t:Token.t) (et:Token.t List.t): unit =
  let rec f () =
    let tok = peek ps in
    match (List.find ~f:(fun t -> phys_equal t tok) (t::et)) with
    | Some _ -> ()
    | None -> bump ps; f ()
  in
  f ()


let get_loc (ps:env) = ()
let token_error ps err = ps.errors <- List.cons err ps.errors
let expect ps tok =
  if phys_equal (peek ps) tok then bump ps else token_error ps (Parse_error.UnexpectedToken (Token.to_string tok))

let ident ps = match (peek ps) with
  | Token.IDENT s -> bump ps; s
  | t -> token_error ps (Parse_error.UnexpectedToken (Token.to_string t)); "$ERROR"

let one_or_more (sep:Token.t) (prule:env -> 'a) (ps:env) : 'a array =
  let accum = ref [prule ps] in
  while phys_equal (peek ps) sep do
    bump ps;
    accum := (prule ps) :: !accum
  done;
  !accum |> List.rev |> Array.of_list

let bracketed_seq (mandatory:int) (bra:Token.t) (ket:Token.t) (sepOpt: Token.t option) (prule:env -> 'a) (ps:env) : 'a array =
  expect ps bra;
  let accum = ref [] in
  let do_sep _ =
    match (sepOpt) with
      None -> ()
    | Some tok -> if (!accum = []) then () else expect ps tok
  in while mandatory > List.length (!accum) do
       do_sep ();
       accum := (prule ps) :: (!accum)
     done;
     while (not (peek ps = ket)) do
       do_sep ();
       accum := (prule ps) :: (!accum)
     done;
     expect ps ket;
     !accum |> List.rev |> Array.of_list

let bracketed_zero_or_more
    (bra:Token.t)
    (ket:Token.t)
    (sepOpt:Token.t Option.t)
    (prule:env -> 'a)
    (ps:env)
    : 'a array =
  bracketed_seq 0 bra ket sepOpt prule ps

let paren_comma_list (prule:env -> 'a) (ps:env) : 'a array =
  bracketed_zero_or_more Token.LPAREN Token.RPAREN (Option.some Token.COMMA) prule ps

(* 4 Instantiations *)

(* 4.1.1 Module instantiation *)

(* 4.1.2 Interface instantiation *)



(* 1.3 Module ports *)
let parameter_port_declaration (ps:env) = skip_until ps Token.COMMA [Token.RPAREN]; ""
let parameter_port_list (ps:env) : string List.t =
  match (peek ps) with
  | Token.POUND -> bump ps;
                   let res = paren_comma_list parameter_port_declaration ps in
                   Array.to_list res
  | _ -> []

let constant_expression ps = ""

let packed_dimension ps =
  let range ps =
    let e1 = constant_expression ps in
    expect ps Token.COLON;
    let e2 = constant_expression ps in
    (e1,e2) in
  expect ps Token.LBRACKET;
  if phys_equal (peek ps) Token.RBRACKET
  then (bump ps; [])
  else (let r = range ps in
       expect ps Token.RBRACKET;
       [r])

(* 8.4 Primaries *)
let time_unit = ""

let primary_literal = ""
let constant_concatination = ""
let constant_multiple_concatination = ""
let constant_function_call = ""
let constant_let_expression = ""
let formal_port_identifier = ""
let constant_primary ps = ""


let primary_literal = ""

(* 8.3 Expressions *)
let constant_expression ps = ""


(* 2.1.2 port declarations *)
let port_declaration (ps:env) = match (peek ps) with
  | Token.INOUT -> ""
  | Token.INPUT -> ""
  | Token.OUTPUT -> ""
  | tok -> token_error ps (Parse_error.UnexpectedToken (Token.to_string tok)); bump ps; ""

let list_of_port_declarations (ps:env) =
  match (peek ps) with
  | Token.LPAREN -> let res = paren_comma_list port_declaration ps in
                    Array.to_list res
  | _ -> []

let module_item (ps:env) = skip_until ps Token.SEMICOLON [Token.ENDMODULE]
let module_items (ps:env) = match (peek ps) with
  | Token.ENDMODULE -> []
  | _ -> one_or_more Token.SEMICOLON module_item ps |> Array.to_list





(* system verilog source text *)
let module_declaration (ps:env) =
  expect ps Token.MODULE;
  let name = ident ps in
  let params = parameter_port_list ps in
  let ports = list_of_port_declarations ps in
  expect ps Token.SEMICOLON;
  (* let items = module_items ps in *)
  expect ps Token.ENDMODULE;
  (name, params, ports)
