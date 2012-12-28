open Environment;;
open Environment_gcc;;

let gcc_command = ref "gcc";;

let rec parse_args i = (
	if i < Array.length Sys.argv then (
		begin match Sys.argv.(i) with
		| "--gcc" ->
			gcc_command := Sys.argv.(i + 1);
			parse_args (i + 2)
		| arg ->
			prerr_string ("invalid option:" ^ arg);
			prerr_newline ();
			parse_args (i + 1)
		end
	)
) in
parse_args 1;;

let print_env (env: environment): unit = (
	print_string "---- target ----\n";
	print_string env.en_target;
	print_newline ();
	print_string "---- predefined ----\n";
	print_string env.en_predefined;
	print_string "---- include ----\n";
	List.iter (fun i -> print_string i; print_newline ()) env.en_include;
	print_string "---- sys include ----\n";
	List.iter (fun i -> print_string i; print_newline ()) env.en_sys_include
);;

let env = gcc_env !gcc_command `c in
print_string ("* " ^ !gcc_command ^ " *\n");
print_env env;;
