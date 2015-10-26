include Core.Std

type filename = string
type position = filename * int * int
type pstate =
  { mutable pstate_peek  : Token.t;
    mutable pstate_ctxt  : (string * position) list;
    mutable pstate_depth : int;
    pstate_lexbuf        : Lexing.lexbuf;
    pstate_file          : filename; }

let make_parser
      (fname:string) : pstate =
  let lexbuf = Lexing.from_channel (open_in fname) in
  let source_position = { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = fname } in
  let current_position = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname } in
  lexbuf.Lexing.lex_start_p <- source_position;
  lexbuf.Lexing.lex_curr_p <- current_position;
  let first_token = Lexer.token lexbuf in
  let ps = {
      pstate_peek   = first_token;
      pstate_ctxt   = [];
      pstate_depth  = 0;
      pstate_lexbuf = lexbuf;
      pstate_file   = fname;
    } in
  ps

let peek (ps:pstate) : Token.t =
  ps.pstate_peek

let bump (ps:pstate) : unit =
  ps.pstate_peek <- Lexer.token ps.pstate_lexbuf

let expect (ps:pstate) (t:Token.t) : unit =
  let p = peek ps in
  if phys_equal p t then bump ps else
    let msg = ("Expected " ^ Token.string_of_token t ^ " found " ^ Token.string_of_token p) in
    begin
      print_string msg;
    end

let parse_ident (ps:pstate) : Ast.L0.Term.t =
  let token = peek ps in
  match token with
  | Token.IDENT id ->
     bump ps;
     Ast.L0.Term.Variable id
  | _ -> let msg = ("Expected identifier found " ^ Token.string_of_token token)  ^ "\n" in
         begin
           print_string msg;
           assert false
         end

let rec parse_term (ps:pstate) : Ast.L0.Term.t = match peek ps with
  |  Token.IDENT id ->
      bump ps;
      Ast.L0.Term.Variable id
  | Token.LPAREN ->
     let tm = parse_term ps in
     expect ps Token.RPAREN;
     tm
  | Token.RBRACE ->
     let tm = parse_term ps in
     expect ps Token.RBRACE;
     tm
  | Token.RBRACKET ->
     let tm = parse_term ps in
     expect ps Token.RBRACKET;
     tm
  | Token.LET -> parse_let ps
  | Token.FUN -> parse_fun ps
  | _ -> assert false
and parse_application (ps:pstate) : Ast.L0.Term.t =
  let fn = parse_ident ps in
  let var = parse_ident ps in
  Ast.L0.Term.Application (fn, var)

and parse_let (ps:pstate) : Ast.L0.Term.t =
  expect ps (Token.LET);
  let Ast.L0.Term.Variable v = parse_ident ps in
  expect ps (Token.EQUALS);
  let term = parse_term ps in
  expect ps (Token.IN);
  let body = parse_term ps in
  Ast.L0.Term.Let (v, term, body)
and parse_fun (ps:pstate) : Ast.L0.Term.t =
  expect ps (Token.FUN);
  let Ast.L0.Term.Variable v = parse_ident ps in
  expect ps (Token.ARROW);
  let term = parse_term ps in
  Ast.L0.Term.Lambda (v, term)

let rec parse_term_list (ps:pstate) (tl:Ast.L0.Term.t list) (tok : Token.t) : Ast.L0.Term.t list =
  let token = peek ps in
  if token = tok then tl else
    let tm = parse_term ps in
    let tl = List.append  tl  [tm] in
    parse_term_list ps tl tok

let rec parse_module (ps:pstate) : Ast.L0.Module.t =
  expect ps (Token.MODULE);
  let tl = parse_term_list ps [] Token.EOF in
  Ast.L0.Module.Module tl
