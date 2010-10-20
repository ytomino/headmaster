open Environment;;
open Environment_gcc;;

let print_env (env: environment): unit = (
	print_string "---- predefined ----\n";
	print_string env.en_predefined;
	print_string "---- include ----\n";
	List.iter (fun i -> print_string i; print_newline ()) env.en_include;
	print_string "---- sys include ----\n";
	List.iter (fun i -> print_string i; print_newline ()) env.en_sys_include
);;

let env = gcc_env "gcc" `c in
print_string "* gcc *\n";
print_env env;;

print_newline ();;

let env = gcc_env "i686-pc-freebsd7-gcc" `c in
print_string "* i686-pc-freebsd7-gcc *\n";
print_env env;;
