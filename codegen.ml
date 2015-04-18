module Ast = Ast.L1

exception Error of string
type codegen_state = {
  llvm_context : Llvm.llcontext;
  llvm_module  : Llvm.llmodule;
  llvm_builder : Llvm.llbuilder;
  named_values : (string, Llvm.llvalue) Hashtbl.t;
}

let codegen_literal = fun ctx -> function
  | Ast.Term.Double d -> Llvm.const_float (Llvm.double_type ctx.llvm_context) d
  | Ast.Term.Integer i -> Llvm.const_int (Llvm.integer_type ctx.llvm_context 32) i

let rec codegen_term = fun ctx -> function
  | Ast.Term.Literal l -> codegen_literal ctx l
  | Ast.Term.Variable name ->
    (try Hashtbl.find ctx.named_values name with
     | Not_found -> raise (Error "unknown variable name"))
  | Ast.Term.Binary (op,t,x,y) ->
    let x' = codegen_term ctx x in
    let y' = codegen_term ctx y in
    begin
      match op with
      | "+" -> Llvm.build_fadd x' y' "addtmp" ctx.llvm_builder
      | "-" -> Llvm.build_fsub x' y' "subtmp" ctx.llvm_builder
      | "*" -> Llvm.build_fmul x' y' "multmp" ctx.llvm_builder
      | _ -> raise (Error "invalid binary operation")
    end
  | Ast.Term.Call (callee, args) ->
    let callee =
      match Llvm.lookup_function callee ctx.llvm_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function reference")
    in
    let params = Llvm.params callee in
    if Array.length params == Array.length args then () else
      raise (Error "incorrect # arguments passed");
    let args = Array.map (codegen_term ctx) args in
    Llvm.build_call callee args "calltmp" ctx.llvm_builder

let codegen_proto = fun ctx -> function
  | Ast.Prototype (name,args) ->
    let doubles = Array.make (Array.length args) (Llvm.double_type ctx.llvm_context) in
    let ft = Llvm.function_type (Llvm.double_type ctx.llvm_context) doubles in
    let f =
      match Llvm.lookup_function name ctx.llvm_module with
      | None -> Llvm.declare_function name ft ctx.llvm_module
      | Some f ->
	if Llvm.block_begin f <> Llvm.At_end f then
	  raise (Error "redefinition of function");
	if Llvm.element_type (Llvm.type_of f) <> ft then
	  raise (Error "redefinition of function with different # args");
	f
    in
    Array.iteri (fun i a ->
	let n = args.(i) in
	Llvm.set_value_name n a;
	Hashtbl.add ctx.named_values n a;
      ) (Llvm.params f);
    f
let codegen_function ctx = function
  | Ast.Function (proto, body) ->
    Hashtbl.clear ctx.named_values;
    let the_function = codegen_proto ctx proto in
    let bb = Llvm.append_block ctx.llvm_context "entry" the_function in
    Llvm.position_at_end bb ctx.llvm_builder;
    try
      let ret_val = codegen_term ctx body in
      let _ = Llvm.build_ret ret_val ctx.llvm_builder in
      the_function
    with e ->
      Llvm.delete_function the_function;
      raise e
