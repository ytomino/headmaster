open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	ocaml_lib ~extern:true ~dir:("../build") "gmp";
	(* ocaml_lib ~extern:true ~dir:("../build") "mpfr"; *)
	ocaml_lib ~extern:true ~dir:("../build") "unicode";
	tag_file "hmcpp.ml" [
		"use_gmp";
		"use_unicode"];
	tag_file "hmcpp.native" [
		"use_bigarray";
		"use_gmp";
		"use_unicode";
		"use_unix"]
| _ ->
	()
end;;
