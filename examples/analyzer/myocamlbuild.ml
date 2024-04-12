open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	tag_file "test_analyzer.ml" [];
	tag_file "test_analyzer.byte" [
		"use_unix"]
| _ ->
	()
end;;
