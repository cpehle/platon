open Ocamlbuild_plugin;;

ocaml_lib ~extern:true "llvm";;
ocaml_lib ~extern:true "llvm_analysis";;
ocaml_lib ~extern:true "llvm_executionengine";;
ocaml_lib ~extern:true "llvm_target";;
ocaml_lib ~extern:true "llvm_scalar_opts";;

flag ["link"; "ocaml"; "c++"] (S[A"-cc"; A"c++"]);;

dispatch begin function
             | After_options ->
      Options.ocamldoc := S[A"ocamldoc"; A"-keep-code"; A"-colorize-code"]
  | _ -> ()
end
