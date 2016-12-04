open Core.Std

let lex_all code =
  let lexbuf = Lexbuf.from_string code in
  let rec f acc =
    match Lex.token lexbuf with
    | Token.EOF -> acc
    | tok -> f (tok :: acc)
  in f [] |> List.rev

let init_parser code =
  let lexbuf = Lexbuf.from_string code in
  let tok = Lex.token lexbuf in
  { Vparse.env_peek = tok; Vparse.env_lexbuf = lexbuf; Vparse.errors = []}

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

let _ =
  "(a, b, c, d, e)"
  |> init_parser
  |> Vparse.paren_comma_list Vparse.ident
  |> Array.to_list
  |> String.concat ~sep:" "
  |> print_string;
  "#(a, b, c, d, e)"
  |> init_parser
  |> Vparse.parameter_port_list
  |> String.concat ~sep:" "
  |> print_string;
  "module test #()(); endmodule"
  |> init_parser
  |> Vparse.module_declaration
  |> fun (name,params,ports) ->
     print_string name;
