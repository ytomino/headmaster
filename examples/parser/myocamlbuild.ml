open Ocamlbuild_plugin;;

dispatch begin function
| After_rules ->
	tag_file "test_parser.ml" [];
	tag_file "test_parser.byte" [
		"use_unix"]
| _ ->
	()
end;;
