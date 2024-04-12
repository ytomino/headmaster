open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	tag_file "test_preprocessor.ml" [];
	tag_file "test_preprocessor.byte" [
		"use_unix"]
| _ ->
	()
end;;
