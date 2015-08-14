open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	(* ocaml_lib ~extern:true ~dir:(".") "gmp"; *)
	(* ocaml_lib ~extern:true ~dir:(".") "mpfr"; *)
	(* ocaml_lib ~extern:true ~dir:(".") "unicode"; *)
	tag_file "test_analyzer.ml" [];
	tag_file "test_analyzer.byte" [
		"use_bigarray";
		"use_unix"]
| _ ->
	()
end;;
