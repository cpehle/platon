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
  if p == t then bump ps else assert false
