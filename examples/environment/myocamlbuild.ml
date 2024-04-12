open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	tag_file "test_environment.ml" [];
	tag_file "test_environment.byte" [
		"use_unix"]
| _ ->
	()
end;;
