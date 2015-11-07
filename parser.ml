open Core.Std

type filename = string
type error_msg = string

type env =
  {
    mutable env_peek  : (Token.t, Parse_error.t * Position.t) Result.t;
    mutable env_depth : int;
    on_error : (env -> Error.t -> unit) option;
    env_lexbuf        : Lexing.lexbuf;
    env_file          : filename; }

let make_parser_from_string  (s:string) : env =
  let lexbuf = Lexing.from_string s in
  let source_position = { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = "(interactive)" } in
  let current_position = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "(interactive)" } in
  lexbuf.Lexing.lex_start_p <- source_position;
  lexbuf.Lexing.lex_curr_p <- current_position;
  let first_token = Lexer.token lexbuf in
  let ps = {
      on_error = None;
      env_peek   = first_token;
      env_depth  = 0;
      env_lexbuf = lexbuf;
      env_file   = "interactive";
    } in
  ps

let make_parser
      (fname:string) : env =
  let lexbuf = Lexing.from_channel (open_in fname) in
  let source_position = { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = fname } in
  let current_position = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname } in
  lexbuf.Lexing.lex_start_p <- source_position;
  lexbuf.Lexing.lex_curr_p <- current_position;
  let first_token = Lexer.token lexbuf in
  let ps = {
      on_error = None;
      env_peek   = first_token;
      env_depth  = 0;
      env_lexbuf = lexbuf;
      env_file   = fname;
    } in
  ps

let peek (ps:env) : (Token.t, Parse_error.t * Position.t) Result.t =
  ps.env_peek

let bump (ps:env) : unit =
  ps.env_peek <- Lexer.token ps.env_lexbuf

let expect (ps:env) (t:Token.t) : (unit, Parse_error.t * Position.t) Result.t =
  let open Result.Monad_infix in
  peek ps >>= fun token ->
  if phys_equal token t
  then let () = bump ps in Result.return ()
  else let pos =  (ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_fname, ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum, 0) in
       Result.Error (Parse_error.UnexpectedTokenExpected (token, t), pos)

let ident (ps:env) : (Ast.L0.Term.t, Parse_error.t * Position.t) Result.t =
  let open Result.Monad_infix in
  peek ps >>= fun token ->
  match token with
      | Token.IDENT id -> let _ = bump ps in
                          Result.return (Ast.L0.Term.Variable id)
      | _ as token ->
         let pos =  (ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_fname, ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum, 0) in
         Result.Error (Parse_error.UnexpectedTokenWithExpectation (Token.to_string token, "identifier"), pos)



let rec ptype (ps:env) : (Ast.L0.Type.t, Parse_error.t * Position.t) Result.t =
  let open Result.Monad_infix in
  peek ps >>= fun token -> match token with
  | Token.IDENT s -> ptype0 (Ast.L0.Type.QVar s) ps
  | Token.LPAREN ->
     ptype ps >>= fun t ->
     expect ps Token.LPAREN >>= fun _ ->
     Result.return t
  | _ as token ->
     let pos =  (ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_fname, ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum, 0) in
     Result.Error (Parse_error.UnexpectedToken (Token.to_string token), pos)
and ptype0 t1 ps =
  let open Result in
  let open Result.Monad_infix in
  peek ps >>= fun token -> match token with
                           | Token.ARROW ->
                              ptype ps >>= fun t2 -> return  (Ast.L0.Type.TArrow (t1, t2, Ast.L0.Type.default_levels))
                           | _ -> return t1

let rec term (ps:env) : (Ast.L0.Term.t, Parse_error.t * Position.t) Result.t =
  let open Result in
  let open Result.Monad_infix in
  peek ps >>= fun token -> match token with
  |  Token.IDENT id ->
      bump ps;
      return  (Ast.L0.Term.Variable id)
  | Token.LPAREN ->
     term ps >>= fun tm ->
     expect ps Token.RPAREN >>=  fun _ ->
     return tm
  | Token.RBRACE ->
     term ps >>= fun tm ->
     expect ps Token.RBRACE >>= fun _ ->
     Result.return tm
  | Token.RBRACKET ->
     term ps >>= fun tm ->
     expect ps Token.RBRACKET >>= fun _ ->
     return tm
  | Token.LET -> plet ps
  | Token.FUN -> pfun ps
  | Token.FLOAT f -> Result.return (Ast.L0.Term.Literal (Ast.L0.Term.Literal.Float f))
  | Token.INT i ->  Result.return (Ast.L0.Term.Literal (Ast.L0.Term.Literal.Int i))
  | _ as t ->
     let pos =  (ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_fname, ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum, 0) in
     Error (Parse_error.UnexpectedToken (Token.to_string t), pos)


and application (ps:env) : (Ast.L0.Term.t, Parse_error.t * Position.t) Result.t =
  let open Result.Monad_infix in
  ident ps >>= fun fn ->
  ident ps >>= fun var ->
  Result.return (Ast.L0.Term.Application (fn, var))

and plet (ps:env) : (Ast.L0.Term.t, Parse_error.t * Position.t) Result.t =
  let open Result.Monad_infix in
  expect ps (Token.LET) >>= fun _ ->
  ident ps >>= function
  | (Ast.L0.Term.Variable v) ->
     expect ps (Token.EQUALS) >>= fun _ ->
     term ps >>= fun tm ->
     expect ps (Token.IN) >>= fun _ ->
     term ps >>= fun body ->
     Result.return (Ast.L0.Term.Let (v, tm, body))
  | _ ->
     let pos =  (ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_fname, ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum, 0) in
     Result.Error (Parse_error.InternalError, pos)
(*
varlist:
    var [: type]
identlist:
    varlist*
pfun:
    varlist . term
*)
and pfun (ps:env) : (Ast.L0.Term.t, Parse_error.t * Position.t) Result.t =
  let open Result.Monad_infix in
  expect ps (Token.FUN) >>= fun _ ->
  ident ps >>= function
  | Ast.L0.Term.Variable v ->
     expect ps (Token.DOT) >>= fun _ ->
     term ps >>= fun tm ->
     Result.return (Ast.L0.Term.Lambda (v, tm))
  | _  ->
     let pos =  (ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_fname, ps.env_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum, 0) in
     Result.Error (Parse_error.InternalError, pos)

let rec term_list (ps:env) end_token tl =
  let open Result.Monad_infix in
  peek ps >>= fun token ->
  if token = end_token
  then Result.return tl
  else
    term ps >>= fun tm ->
    let tl = List.append  tl  [tm] in
    term_list ps end_token tl

let rec pmodule (ps:env) =
  let open Result.Monad_infix in
  expect ps Token.MODULE >>= fun _ ->
  ident ps >>= fun mod_ident ->
  term_list ps Token.EOF []  >>= fun tl ->
  Result.return (Ast.L0.Module.Module (mod_ident, tl))
