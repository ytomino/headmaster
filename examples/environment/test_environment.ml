open Environment;;
open Environment_gcc;;

let gcc_command = ref "gcc";;

if not !Sys.interactive then (
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
	parse_args 1
);;

let print_fp_repr (fp_repr: fp_repr): unit = (
	let `mantissa mantissa, `emin emin = fp_repr in
	print_string "mantissa: ";
	print_int mantissa;
	print_string ", emin: ";
	print_int emin;
	print_newline ()
);;

let print_env (env: environment): unit = (
	print_string "---- target ----\n";
	print_string env.en_target;
	print_newline ();
	print_string "---- floating point types ----\n";
	let float_repr, double_repr, long_double_repr = env.en_fp in
	print_fp_repr float_repr;
	print_fp_repr double_repr;
	print_fp_repr long_double_repr;
	print_string "---- predefined ----\n";
	print_string env.en_predefined;
	print_string "---- iquote ----\n";
	List.iter (fun i -> print_string i; print_newline ()) env.en_iquote;
	print_string "---- include ----\n";
	List.iter (fun i -> print_string i; print_newline ()) env.en_include;
	print_string "---- isystem ----\n";
	List.iter (fun i -> print_string i; print_newline ()) env.en_isystem;
	print_string "---- inlining ---\n";
	print_string (if env.en_gnu_inline then "GNU mode" else "C99 mode");
	print_newline ();
);;

let env = gcc_env !gcc_command ~nostdinc:false ~x:`c in
print_string ("* " ^ !gcc_command ^ " *\n");
print_env env;;
