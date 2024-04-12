open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	ocaml_lib ~extern:true ~dir:(".") "gmp";
	ocaml_lib ~extern:true ~dir:(".") "mpfr";
	ocaml_lib ~extern:true ~dir:(".") "unicode";
	tag_file "main.ml" [
		"use_gmp";
		"use_mpfr";
		"use_unicode"];
	tag_file "main.native" [
		"use_gmp";
		"use_mpfr";
		"use_unicode";
		"use_unix"]
| _ ->
	()
end;;
