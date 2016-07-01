include Core.Std

module Source = Llang

type codegen_state = {
    llcontext : Llvm.llcontext;
    llmodule  : Llvm.llmodule;
    llbuilder : Llvm.llbuilder;
    named_values : (string, Llvm.llvalue) Hashtbl.t;
  }

let (>>) f g x = g (f x)
let (|>) x f   = f x

type 'a func = { func : Llvm.llvalue; context : codegen_state }

let cgfunction context name returns args f =
    let fn_type = Llvm.function_type returns args in
    let fn = Llvm.declare_function name fn_type context.llmodule in
    let bb = Llvm.append_block context.llcontext "entry" fn in
    Llvm.position_at_end bb context.llbuilder;
    f {func = fn; context }

let rec find_type_representation : codegen_state -> Source.Type.t -> Llvm.lltype =
    fun ctx -> function
        | Source.Type.Base bt ->
           begin match bt with
                 | Source.Type.Double ->
                    (Llvm.double_type ctx.llcontext)
                 | Source.Type.Integer -> (Llvm.integer_type ctx.llcontext 32) end
        | Source.Type.Arrow (t,t') -> (Llvm.integer_type ctx.llcontext 32)

let cgstruct context array =
    Llvm.struct_type context array


type operator = Add | Mul | Sub
type base_type = Float | Int
type operand = {
    base_type : base_type;
    value : Llvm.llvalue;
  }

let literal = fun context -> function
    | Source.Term.Double d -> {base_type = Float; value = Llvm.const_float (Llvm.double_type context.llcontext) d}
    | Source.Term.Integer i -> {base_type = Int; value = Llvm.const_int (Llvm.integer_type context.llcontext 32) i}

let select context res = ()

let cgbinop (context:codegen_state) (op:operator) (o:operand) (o':operand) : operand = let builder, tmp = match op with
  | Add -> if o.base_type = Float then Llvm.build_fadd, "faddtmp" else  Llvm.build_add, "addtmp"
  | Mul -> if o.base_type = Float then Llvm.build_fmul, "fmultmp" else  Llvm.build_fadd, "multmp"
  | Sub -> if o.base_type = Float then Llvm.build_fsub, "fmultmp" else  Llvm.build_fsub, "subtmp"
 in { base_type = o.base_type; value = builder o.value o'.value tmp context.llbuilder }



let cgbinoplist (context:codegen_state) (ops:operator List.t) (operands : operand List.t) : operand List.t =
  let open List in
  let rec go ops operands acc =
    match ops with
    | [] -> acc
    | o::operators -> match operands with
                      | o1::o2::operands -> go operators operands (cgbinop context o o1 o2::acc)
                      | _ -> assert false
  in go ops operands []


let test_cgbinoplist c op =
  let open Source.Term in
  let ops = [Add; Add] in
  let operands = [literal c (Double 1.0); literal c (Double 0.3); literal c (Double 0.4); op] in
  let r = cgbinoplist c ops operands in
  cgbinoplist c [Add] r

let rec term : codegen_state -> Source.Term.t -> (Llvm.llvalue, Codegen_error.t) Result.t =
  let open Result.Monad_infix in
  fun context ->
  function
  | Source.Term.Literal l -> Result.Ok (literal context l).value
  | Source.Term.Variable name -> begin
      match Hashtbl.find context.named_values name with
      | Some r -> Result.Ok r
      | None -> Result.Error (Codegen_error.Error ("Unbound variable: " ^ name))
    end
  | Source.Term.Binary (op,t,x,y) ->
    term context x >>= fun x' ->
    term context y >>= fun y' ->
    let llvm_ty = find_type_representation context t in
    let o1 = { base_type = Float; value = x' } in
    let o2 = { base_type = Float; value = y' } in
    begin
      match op with
      | "+" -> Result.Ok (cgbinop context Add o1 (List.hd_exn (test_cgbinoplist context o2))).value
      | "-" -> Result.Ok (cgbinop context Sub o1 o2).value
      | "*" -> Result.Ok (cgbinop context Mul o1 o2).value
      | _ -> Result.Error (Codegen_error.Error "invalid binary operation")
    end
  | Source.Term.Call (callee, args) ->
    let callee =
      match Llvm.lookup_function callee context.llmodule with
      | Some callee -> Result.Ok callee
      | None -> Result.Error (Codegen_error.Error "unknown function reference")
    in
    callee >>= fun callee ->
    let params = Llvm.params callee in
    if phys_equal (Array.length params) (Array.length args) then
      Result.Error (Codegen_error.Error "incorrect # arguments passed")
    else
      Result.all (Array.to_list (Array.map args ~f:(term context))) >>| List.to_array >>= fun args ->
      Result.Ok (Llvm.build_call callee args "calltmp" context.llbuilder)


let codegen_proto : codegen_state -> Source.Term.proto -> (Llvm.llvalue, Codegen_error.t) Result.t =
  let open Result.Monad_infix in
  fun context -> function
  | Source.Term.Prototype (name,args) ->
      let doubles = Array.create ~len:(Array.length args) (Llvm.double_type context.llcontext) in
      let ft = Llvm.function_type (Llvm.double_type context.llcontext) doubles in
      (match Llvm.lookup_function name context.llmodule with
       | Some f when Llvm.block_begin f <> Llvm.At_end f -> Result.Error (Codegen_error.Error "redefinition of function")
       | Some f when Llvm.element_type (Llvm.type_of f) <> ft -> Result.Error (Codegen_error.Error  "redefinition of function with different # args")
       | None -> Result.Ok (Llvm.declare_function name ft context.llmodule)
       | Some f -> Result.Ok f)
      >>= fun f -> Array.iteri
                     ~f:(fun i a ->
                         let n = args.(i) in
                         Llvm.set_value_name n a;
                         match Hashtbl.add context.named_values n a with
                         | `Ok
                         | `Duplicate -> ()
                        )
                     (Llvm.params f);
                   Result.Ok f

let codegen_function context =
  let open Result.Monad_infix in function
  | Source.Term.Function (proto, body) ->
      Hashtbl.clear context.named_values;
      codegen_proto context proto >>= fun the_function ->
      let bb = Llvm.append_block context.llcontext "entry" the_function in
      Llvm.position_at_end bb context.llbuilder;
      term context body >>= fun ret_val ->
      let _ = Llvm.build_ret ret_val context.llbuilder in
      Llvm_analysis.assert_valid_function the_function;
      Result.return the_function

