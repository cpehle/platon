open Core.Std
open Ast.L0

type filename = string
type error_msg = string

type env = {
    mutable env_peek : (Token.t, Parse_error.t * Position.t * Position.t) Result.t;
    mutable env_depth : int;
    on_error : (env -> Error.t -> unit) option;
    env_lexbuf : Lexbuf.t;
    env_file : filename;
  }

let from_lexbuf lexbuf : env =
  let  first_token = Plexer.token lexbuf in
  let ps = {
      on_error   = None;
      env_peek   = first_token;
      env_depth  = 0;
      env_lexbuf = lexbuf;
      env_file   = "<none>";
    } in ps

let from_string  (s:string) : env =
  Lexbuf.from_string s |> from_lexbuf

let from_file (f:string) : env =
  In_channel.with_file f ~f:(fun channel ->
  Lexbuf.from_channel channel |> from_lexbuf)

let fail (ps:env) perror = Result.Error (perror, ps.env_lexbuf.Lexbuf.pos_start, ps.env_lexbuf.Lexbuf.pos_end)
let peek (ps:env) : (Token.t, Parse_error.t * Position.t * Position.t) Result.t = ps.env_peek
let bump (ps:env) : unit = ps.env_peek <- Plexer.token ps.env_lexbuf

let expect (ps:env) (t:Token.t) : (unit, Parse_error.t * Position.t * Position.t) Result.t =
  let open Result.Monad_infix in
  peek ps >>= fun token ->
  if phys_equal token t
  then begin bump ps; Result.return () end
  else fail ps (Parse_error.UnexpectedTokenExpected (token, t))

let ident (ps:env) : (string * Position.t * Position.t, Parse_error.t * Position.t * Position.t) Result.t =
  let open Result.Monad_infix in
  peek ps >>= fun token ->
  match token with
      | Token.IDENT id -> bump ps;
                          Result.return (id, ps.env_lexbuf.Lexbuf.pos_start, ps.env_lexbuf.Lexbuf.pos_end)
      | _ as token -> fail ps (Parse_error.UnexpectedTokenWithExpectation (Token.to_string token, "identifier"))

let rec ptype (ps:env) : (Type.t, Parse_error.t * Position.t * Position.t) Result.t =
  let open Result.Monad_infix in
  peek ps >>= fun token -> match token with
  | Token.IDENT s -> ptype0 (Type.QVar s) ps
  | Token.LPAREN ->
     ptype ps >>= fun t ->
     expect ps Token.LPAREN >>= fun _ ->
     Result.return t
  | _ as token -> fail ps  (Parse_error.UnexpectedToken (Token.to_string token))
and ptype0 t1 ps =
  let open Result in
  let open Result.Monad_infix in
  peek ps >>= fun token -> match token with
                           | Token.RIGHTARROW ->
                              ptype ps >>= fun t2 -> return  (Type.TArrow (t1, t2, Type.default_levels))
                           | _ -> return t1

let rec term (ps:env) : (Term.t, Parse_error.t * Position.t * Position.t) Result.t =
  let open Result.Monad_infix in
  peek ps >>= function
  | Token.IDENT id ->
     ident ps >>= fun (id,p,p') ->
     Result.return  (Term.Variable id)
  | Token.FLOAT f -> bump ps; Result.return (Term.Literal (Term.Literal.Float f))
  | Token.INT i ->  bump ps; Result.return (Term.Literal (Term.Literal.Int i))
  | Token.LPAREN ->
     bump ps;
     term_list ps Token.RPAREN [] >>= fun tms ->
     Result.return (Term.Comp tms)
  | Token.LBRACE ->
     bump ps;
     term_list ps Token.RBRACE [] >>= fun tms ->
     Result.return (Term.Rel tms)
  | Token.LBRACKET ->
     bump ps;
     term_list ps Token.RBRACKET [] >>= fun tms ->
     Result.return (Term.Prod tms)
  | Token.LET -> plet ps
  | Token.FUN -> pfun ps
  | _ as t -> fail ps (Parse_error.UnexpectedToken (Token.to_string t))
and application (ps:env) : (Term.t, Parse_error.t * Position.t * Position.t) Result.t =
  let open Result.Monad_infix in
  ident ps >>| (fun (id,p,p') -> id) >>| Term.var >>= fun fn ->
  ident ps >>| (fun (id,p,p') -> id) >>| Term.var >>= fun var -> Result.return (Term.Application (fn, var))
and plet (ps:env) : (Term.t, Parse_error.t * Position.t * Position.t) Result.t =
  let open Result.Monad_infix in
  let start_pos = ps.env_lexbuf.Lexbuf.pos_start in
  expect ps (Token.LET) >>= fun _ ->
  ident ps >>| (fun (id,p,p') -> id) >>= fun v ->
  expect ps (Token.EQUALS) >>= fun _ ->
  term_list ps (Token.IN) [] >>| Term.prod >>= fun tm ->
  expect ps (Token.IN) >>= fun _ ->
  term ps >>= fun body ->
  let end_pos = ps.env_lexbuf.Lexbuf.pos_end in
  Result.return (Term.Let (v, tm, body))
and pfun (ps:env) : (Term.t, Parse_error.t * Position.t * Position.t) Result.t =
  let open Result.Monad_infix in
  let start_pos = ps.env_lexbuf.Lexbuf.pos_start in
  expect ps (Token.FUN) >>= fun _ ->
  ident ps >>|  (fun (id,p,p') -> id) >>= fun v ->
     expect ps (Token.DOT) >>= fun _ ->
     term ps >>= fun tm ->
     let end_pos = ps.env_lexbuf.Lexbuf.pos_end in
     Result.return (Term.Lambda (v, tm))
and term_list (ps:env) end_token tl =
  let open Result.Monad_infix in
  peek ps >>= function
  | Token.EOF -> fail ps (Parse_error.UnexpectedToken "<eof>")
  | t when t = end_token ->
     bump ps;
     Result.return tl
  | _ -> term ps >>= fun tm ->
    let tl = List.append  tl  [tm] in
    term_list ps end_token tl
