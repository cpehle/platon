include Core.Std
module Source = Ast.L1

exception Error of string


type codegen_state = {
  llcontext : Llvm.llcontext;
  llmodule  : Llvm.llmodule;
  llbuilder : Llvm.llbuilder;
  named_values : (String.t, Llvm.llvalue) Hashtbl.t;
}

let rec find_type_representation : codegen_state -> Source.Type.t -> Llvm.lltype
  = fun ctx -> function
            | Source.Type.Base bt -> begin match bt with
                               | Source.Type.Double -> (Llvm.double_type ctx.llcontext)
                               | Source.Type.Integer -> (Llvm.integer_type ctx.llcontext 32) end
            | Source.Type.Arrow (t,t_) -> (Llvm.integer_type ctx.llcontext 32)


let codegen_struct context array =
  Llvm.struct_type context array

let codegen_literal = fun context -> function
  | Source.Term.Double d -> Llvm.const_float (Llvm.double_type context.llcontext) d
  | Source.Term.Integer i -> Llvm.const_int (Llvm.integer_type context.llcontext 32) i

let rec codegen_term = fun context -> function
  | Source.Term.Literal l -> codegen_literal context l
  | Source.Term.Variable name ->
    (try Hashtbl.find_exn context.named_values name with
     | Not_found -> raise (Error "unknown variable name"))
  | Source.Term.Binary (op,t,x,y) ->
    let x' = codegen_term context x in
    let y' = codegen_term context y in
    let llvm_ty = find_type_representation context t in
    begin
      match op with
      | "+" -> Llvm.build_fadd x' y' "addtmp" context.llbuilder
      | "-" -> Llvm.build_fsub x' y' "subtmp" context.llbuilder
      | "*" -> Llvm.build_fmul x' y' "multmp" context.llbuilder
      | _ -> raise (Error "invalid binary operation")
    end
  | Source.Term.Call (callee, args) ->
    let callee =
      match Llvm.lookup_function callee context.llmodule with
      | Some callee -> callee
      | None -> raise (Error "unknown function reference")
    in
    let params = Llvm.params callee in
    if phys_equal (Array.length params) (Array.length args) then () else
      raise (Error "incorrect # arguments passed");
    let args = Array.map (codegen_term context) args in
    Llvm.build_call callee args "calltmp" context.llbuilder

let codegen_proto = fun context -> function
  | Source.Term.Prototype (name,args) ->
    let doubles = Array.create ~len:(Array.length args) (Llvm.double_type context.llcontext) in
    let ft = Llvm.function_type (Llvm.double_type context.llcontext) doubles in
    let f =
      match Llvm.lookup_function name context.llmodule with
      | None -> Llvm.declare_function name ft context.llmodule
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
    match Hashtbl.add context.named_values n a with
    | `Ok | `Duplicate -> ()
      ) (Llvm.params f);
    f

let codegen_function context = function
  | Source.Term.Function (proto, body) ->
    Hashtbl.clear context.named_values;
    let the_function = codegen_proto context proto in
    let bb = Llvm.append_block context.llcontext "entry" the_function in
    Llvm.position_at_end bb context.llbuilder;
    try
      let ret_val = codegen_term context body in
      let _ = Llvm.build_ret ret_val context.llbuilder in
      Llvm_analysis.assert_valid_function the_function;
      the_function
    with e ->
      Llvm.delete_function the_function;
      raise e
