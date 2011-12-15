open Format;;

type calling_convention = [`cdecl | `stdcall | `fastcall | `thiscall];;
type ada_calling_convention = [calling_convention | `ada | `intrinsic | `c_pass_by_copy];;

let convention_identifier (conv: [< ada_calling_convention]): string = (
	begin match conv with
	| `ada -> "Ada"
	| `intrinsic -> "Intrinsic"
	| `cdecl -> "C"
	| `c_pass_by_copy -> "C_Pass_By_Copy"
	| `stdcall -> "Stdcall"
	| `fastcall -> "Fastcall" (* will be error by gnat *)
	| `thiscall -> "Thiscall" (* will be error by gnat *)
	end
);;

type precedence = [
	| `logical_and
	| `logical_and_then
	| `logical_or
	| `logical_or_else
	| `logical_xor
	| `relation
	| `simple
	| `term
	| `factor
	| `primary];;

type outside_precedence = [
	| precedence
	| `lowest
	| `simple_right
	| `term_right];;

let parenthesis_required
	~(outside: outside_precedence)
	~(inside: precedence)
	: bool =
(
	begin match outside with
	| `lowest ->
		false
	| `logical_and | `logical_and_then
	| `logical_or | `logical_or_else | `logical_xor as outside ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor as inside ->
			inside <> outside
		| `relation | `simple | `term | `factor | `primary ->
			false
		end
	| `relation | `simple ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor
		| `relation ->
			true
		| `simple | `term | `factor | `primary ->
			false
		end
	| `simple_right | `term ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor
		| `relation | `simple ->
			true
		| `term | `factor | `primary ->
			false
		end
	| `term_right ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor
		| `relation | `simple | `term ->
			true
		| `factor | `primary ->
			false
		end
	| `factor ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor
		| `relation | `simple | `term | `factor ->
			true
		| `primary ->
			false
		end
	| `primary ->
		true
	end
);;

let indent = 3;;

(* package *)

type with_option =
	[`limited_with | `none] *
	[`private_with | `none] *
	[`use | `none];;

let narrow_with_option: with_option = `limited_with, `private_with, `none;;

let widely_with_option (a : with_option) (b: with_option): with_option = (
	let a_limited, a_private, a_use = a in
	let b_limited, b_private, b_use = b in
	(if a_limited = `none || b_limited = `none then `none else `limited_with),
	(if a_private = `none || b_private = `none then `none else `private_with),
	(if a_use = `none && b_use = `none then `none else `use)
);;

type with_clause = string * with_option;;

let pp_with_caluse
	(ff: formatter)
	(package, (is_limited, is_private, use): with_clause)
	: unit =
(
	pp_open_box ff indent;
	begin match is_limited with
	| `limited_with ->
		pp_print_string ff "limited ";
	| `none ->
		()
	end;
	begin match is_private with
	| `private_with ->
		pp_print_string ff "private ";
	| `none ->
		()
	end;
	pp_print_string ff "with ";
	pp_print_string ff package;
	pp_print_char ff ';';
	begin match use with
	| `use ->
		fprintf ff "@ use %s;" package
	| `none ->
		()
	end;
	pp_close_box ff ();
	pp_print_break ff 0 0
);;

let pp_package_spec
	(ff: formatter)
	~(with_packages: with_clause list)
	~(name: string)
	~(kind: [`pure | `preelaborate | `normal])
	~(pp_contents: formatter -> unit)
	~(pp_private: (formatter -> unit) option)
	: unit =
(
	pp_open_vbox ff 0;
	List.iter (pp_with_caluse ff) with_packages;
	pp_open_vbox ff indent;
	fprintf ff "package %s is" name;
	begin match kind with
	| `pure ->
		pp_print_space ff ();
		fprintf ff "pragma Pure;"
	| `preelaborate ->
		pp_print_space ff ();
		fprintf ff "pragma Preelaborate;"
	| `normal ->
		()
	end;
	pp_contents ff;
	pp_close_box ff ();
	begin match pp_private with
	| Some pp_private ->
		pp_print_break ff 0 0;
		pp_open_vbox ff indent;
		pp_print_string ff "private";
		pp_private ff;
		pp_close_box ff ()
	| None ->
		()
	end;
	fprintf ff "@ end %s;@," name;
	pp_close_box ff ()
);;

let pp_package_body
	(ff: formatter)
	~(with_packages: with_clause list)
	~(name: string)
	~(pp_contents: formatter -> unit)
	: unit =
(
	pp_open_vbox ff 0;
	List.iter (pp_with_caluse ff) with_packages;
	pp_open_vbox ff indent;
	fprintf ff "package body %s is" name;
	pp_contents ff;
	pp_close_box ff ();
	fprintf ff "@ end %s;@," name;
	pp_close_box ff ()
);;

(* type *)

let pp_incomplete_type
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	pp_print_string ff "type";
	pp_print_space ff ();
	pp_print_string ff name;
	pp_print_char ff ';';
	pp_close_box ff ()
);;

let pp_type
	(ff: formatter)
	(name: string)
	?(pp_discriminants: (formatter -> unit) list = [])
	(pp_definition: formatter -> 'a)
	: 'a =
(
	pp_print_space ff ();
	pp_open_vbox ff indent;
	pp_open_box ff indent;
	pp_print_string ff "type";
	pp_print_space ff ();
	pp_print_string ff name;
	pp_print_space ff ();
	if pp_discriminants <> [] then (
		pp_print_char ff '(';
		List.iter (fun pp_d -> pp_d ff) pp_discriminants;
		pp_print_char ff ')';
		pp_print_space ff ()
	);
	pp_print_string ff "is";
	pp_print_space ff ();
	pp_definition ff
);;

let pp_private_type_declaration
	(ff: formatter)
	(is_limited: [`limited | `none])
	: unit =
(
	begin match is_limited with
	| `limited ->
		pp_print_string ff "limited "
	| `none ->
		()
	end;
	pp_print_string ff "private;";
	pp_close_box ff ();
	pp_close_box ff ()
);;

let pp_derived_type_definition
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_string ff "new ";
	pp_print_string ff name;
	pp_print_char ff ';';
	pp_close_box ff ();
	pp_close_box ff ()
);;

let pp_record_definition
	(ff: formatter)
	(pp_elements: (formatter -> unit) list)
	: unit =
(
	if pp_elements = [] then (
		pp_print_string ff "null record;";
		pp_close_box ff ();
		pp_close_box ff ()
	) else (
		pp_print_string ff "record";
		pp_close_box ff ();
		List.iter (fun pp_e -> pp_e ff) pp_elements;
		pp_close_box ff ();
		pp_print_space ff ();
		pp_print_string ff "end record;"
	)
);;

let pp_subtype
	(ff: formatter)
	(name: string)
	(pp_type: formatter -> unit)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	pp_print_string ff "subtype";
	pp_print_space ff ();
	pp_print_string ff name;
	pp_print_space ff ();
	pp_print_string ff "is";
	pp_print_space ff ();
	pp_type ff;
	pp_print_string ff ";";
	pp_close_box ff ()
);;

(* statement *)

let pp_begin
	(ff: formatter)
	?(label: string = "")
	()
	: unit =
(
	pp_print_space ff ();
	pp_open_vbox ff indent;
	if label <> "" then (
		fprintf ff "%s : " label
	);
	pp_print_string ff "begin"
);;

let pp_end
	(ff: formatter)
	?(label: string = "")
	()
	: unit =
(
	pp_close_box ff ();
	pp_print_space ff ();
	if label <> "" then (
		fprintf ff "end %s;" label
	) else (
		pp_print_string ff "end;"
	)
);;

let pp_if
	(ff: formatter)
	~(pp_cond: formatter -> unit -> unit)
	~(pp_true_case: formatter -> unit -> unit)
	~(pp_false_case: (formatter -> unit -> unit) option)
	: unit =
(
	pp_print_space ff ();
	pp_open_vbox ff indent;
	pp_open_box ff 0;
	pp_open_box ff indent;
	pp_print_string ff "if";
	pp_print_space ff ();
	pp_cond ff ();
	pp_close_box ff ();
	pp_print_space ff ();
	pp_print_string ff "then";
	pp_close_box ff ();
	pp_true_case ff ();
	pp_close_box ff ();
	begin match pp_false_case with
	| Some pp_false_case ->
		pp_print_space ff ();
		pp_open_vbox ff indent;
		pp_print_string ff "else";
		pp_false_case ff ();
		pp_close_box ff ();
	| None ->
		()
	end;
	pp_print_space ff ();
	pp_print_string ff "end if;"
);;

let pp_loop
	(ff: formatter)
	~(pp_cond: (formatter -> unit -> unit) option)
	~(pp_loop: formatter -> unit -> unit)
	: unit =
(
	pp_print_space ff ();
	pp_open_vbox ff indent;
	pp_open_box ff 0;
	begin match pp_cond with
	| Some pp_cond ->
		pp_open_box ff indent;
		pp_cond ff ();
		pp_close_box ff ();
		pp_print_space ff ();
	| None ->
		()
	end;
	pp_print_string ff "loop";
	pp_close_box ff ();
	pp_loop ff ();
	pp_close_box ff ();
	pp_print_space ff ();
	pp_print_string ff "end loop;"
);;

let pp_while
	(pp_cond: formatter -> unit -> unit)
	: formatter -> unit -> unit =
(
	begin fun ff ->
		pp_print_string ff "while";
		pp_print_space ff ();
		pp_cond ff
	end
);;

let pp_exit
	(ff: formatter)
	~(pp_when: (formatter -> unit -> unit) option)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	pp_print_string ff "exit";
	begin match pp_when with
	| Some pp_when ->
		pp_print_space ff ();
		pp_print_string ff "when";
		pp_print_space ff ();
		pp_when ff ()
	| None ->
		()
	end;
	pp_print_char ff ';';
	pp_close_box ff ()
);;

let pp_return
	(ff: formatter)
	(pp_expr: (formatter -> unit -> unit) option)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	pp_print_string ff "return";
	begin match pp_expr with
	| Some pp_expr ->
		pp_print_space ff ();
		pp_expr ff ()
	| None ->
		()
	end;
	pp_print_char ff ';';
	pp_close_box ff ()
);;

let pp_null_statement (ff: formatter) (): unit = (
	pp_print_space ff ();
	pp_open_box ff indent;
	pp_print_string ff "null;";
	pp_close_box ff ()
);;

(* expression *)

let pp_open_paren ff () = (
	pp_print_char ff '(';
	pp_print_break ff 0 0
);;

let pp_close_paren ff () = (
	pp_print_char ff ')'
);;

(* pragma *)

let pp_pragma_complex_representation
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	fprintf ff "pragma Complex_Representation (%s);" name;
	pp_close_box ff ()
);;

let pp_pragma_convention
	(ff: formatter)
	(conv: [< ada_calling_convention])
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	let conv_ident = convention_identifier conv in
	fprintf ff "pragma Convention (%s,@ %s);" conv_ident name;
	pp_close_box ff ()
);;

let pp_pragma_convention_identifier
	(ff: formatter)
	(alias: string)
	(conv: [< ada_calling_convention])
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	let conv_ident = convention_identifier conv in
	fprintf ff "pragma Convention_Identifier (%s,@ %s);" alias conv_ident;
	pp_close_box ff ()
);;

let pp_pragma_import
	(ff: formatter)
	(conv: [< ada_calling_convention])
	(name: string)
	(external_name: string)
	: unit =
(
	let conv_ident = convention_identifier conv in
	pp_print_space ff ();
	pp_open_box ff indent;
	assert (external_name.[0] <> '\"');
	let shorthand =
		match conv with
		| `intrinsic -> name = external_name
		| `ada | `c_pass_by_copy | #calling_convention -> false
	in
	if shorthand then (
		fprintf ff "pragma Import (%s,@ %s);" conv_ident name
	) else (
		fprintf ff "pragma Import (%s,@ %s,@ \"%s\");" conv_ident name external_name
	);
	pp_close_box ff ()
);;

let pp_pragma_inline
	(ff: formatter)
	?(always: bool = false)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	if always then (
		fprintf ff "pragma Inline_Always (%s);" name
	) else (
		fprintf ff "pragma Inline (%s);" name
	);
	pp_close_box ff ()
);;

let pp_pragma_noreturn
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	fprintf ff "pragma No_Return (%s);" name;
	pp_close_box ff ()
);;

let pp_pragma_no_strict_aliasing
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	fprintf ff "pragma No_Strict_Aliasing (%s);" name;
	pp_close_box ff ()
);;

let pp_pragma_pack
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	fprintf ff "pragma Pack (%s);" name;
	pp_close_box ff ()
);;

let pp_pragma_unchecked_union
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	fprintf ff "pragma Unchecked_Union (%s);" name;
	pp_close_box ff ()
);;

let pp_pragma_volatile
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	fprintf ff "pragma Volatile (%s);" name;
	pp_close_box ff ()
);;
