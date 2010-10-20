open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	(* ocaml_lib ~extern:true ~dir:("../build") "gmp"; *)
	(* ocaml_lib ~extern:true ~dir:("../build") "mpfr"; *)
	(* ocaml_lib ~extern:true ~dir:("../build") "unicode"; *)
	tag_file "test_analyzer.ml" [];
	tag_file "test_analyzer.byte" [
		"use_unix"]
| _ ->
	()
end;;
