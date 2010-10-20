open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	ocaml_lib ~extern:true ~dir:("../build") "gmp";
	ocaml_lib ~extern:true ~dir:("../build") "mpfr";
	ocaml_lib ~extern:true ~dir:("../build") "unicode";
	tag_file "main.ml" [
		"use_gmp";
		"use_mpfr";
		"use_unicode"];
	tag_file "main.native" [
		"use_bigarray";
		"use_gmp";
		"use_mpfr";
		"use_unicode";
		"use_unix"]
| _ ->
	()
end;;
