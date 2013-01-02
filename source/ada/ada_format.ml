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
		pp_print_space ff ();
		pp_print_string ff "use ";
		pp_print_string ff package;
		pp_print_char ff ';'
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
	~(pp_contents: formatter -> unit -> unit)
	~(pp_private: (formatter -> unit -> unit) option)
	: unit =
(
	pp_open_vbox ff 0;
	List.iter (pp_with_caluse ff) with_packages;
	pp_open_vbox ff indent;
	pp_print_string ff "package ";
	pp_print_string ff name;
	pp_print_string ff " is";
	begin match kind with
	| `pure ->
		pp_print_space ff ();
		pp_print_string ff "pragma Pure;"
	| `preelaborate ->
		pp_print_space ff ();
		pp_print_string ff "pragma Preelaborate;"
	| `normal ->
		()
	end;
	pp_contents ff ();
	pp_close_box ff ();
	begin match pp_private with
	| Some pp_private ->
		pp_print_break ff 0 0;
		pp_open_vbox ff indent;
		pp_print_string ff "private";
		pp_private ff ();
		pp_close_box ff ()
	| None ->
		()
	end;
	pp_print_break ff 0 0;
	pp_print_string ff "end ";
	pp_print_string ff name;
	pp_print_char ff ';';
	pp_print_break ff 0 0;
	pp_close_box ff ()
);;

let pp_package_body
	(ff: formatter)
	~(with_packages: with_clause list)
	~(name: string)
	~(pp_contents: formatter -> unit -> unit)
	: unit =
(
	pp_open_vbox ff 0;
	List.iter (pp_with_caluse ff) with_packages;
	pp_open_vbox ff indent;
	pp_print_string ff "package body ";
	pp_print_string ff name;
	pp_print_string ff " is";
	pp_contents ff ();
	pp_close_box ff ();
	pp_print_break ff 0 0;
	pp_print_string ff "end ";
	pp_print_string ff name;
	pp_print_char ff ';';
	pp_print_break ff 0 0;
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
	?(pp_discriminants: (formatter -> unit -> unit) list = [])
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
		List.iter (fun pp_d -> pp_d ff ()) pp_discriminants;
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
	(pp_type_name: formatter -> 'a -> unit)
	(type_name: 'a)
	: unit =
(
	pp_print_string ff "new ";
	pp_type_name ff type_name;
	pp_print_char ff ';';
	pp_close_box ff ();
	pp_close_box ff ()
);;

let pp_access_definition
	(ff: formatter)
	(mode: [`constant | `all | `none])
	(pp_target: formatter -> 'a -> 'b)
	(target: 'a)
	: 'b =
(
	pp_print_string ff "access ";
	begin match mode with
	| `constant -> pp_print_string ff "constant ";
	| `all -> pp_print_string ff "all ";
	| `none -> ()
	end;
	let result = pp_target ff target in
	pp_print_char ff ';';
	pp_close_box ff ();
	pp_close_box ff ();
	result
);;

let pp_array_definition
	(ff: formatter)
	(pp_index: formatter -> unit -> unit)
	(aliased: [`aliased | `none])
	(pp_component_type_name: formatter -> 'a -> unit)
	(component_type_name: 'a)
	: unit =
(
	pp_print_string ff "array (";
	pp_index ff ();
	pp_print_string ff ") of";
	pp_print_space ff ();
	begin match aliased with
	| `aliased -> pp_print_string ff "aliased "
	| `none -> ()
	end;
	pp_component_type_name ff component_type_name;
	pp_print_char ff ';';
	pp_close_box ff ();
	pp_close_box ff ()
);;

let pp_record_definition
	(ff: formatter)
	(pp_components: (formatter -> unit -> unit) list)
	: unit =
(
	if pp_components = [] then (
		pp_print_string ff "null record;";
		pp_close_box ff ();
		pp_close_box ff ()
	) else (
		pp_print_string ff "record";
		pp_close_box ff ();
		List.iter (fun pp_e -> pp_e ff ()) pp_components;
		pp_close_box ff ();
		pp_print_space ff ();
		pp_print_string ff "end record;"
	)
);;

let pp_subtype
	(ff: formatter)
	(name: string)
	(pp_type_name: formatter -> 'a -> unit)
	(type_name: 'a)
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
	pp_type_name ff type_name;
	pp_print_string ff ";";
	pp_close_box ff ()
);;

(* object *)

let pp_universal_constant_object
	(ff: formatter)
	(name: string)
	(pp_expr: formatter -> unit -> unit)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	pp_open_box ff indent;
	pp_print_string ff name;
	pp_print_string ff " :";
	pp_print_space ff ();
	pp_print_string ff "constant :=";
	pp_close_box ff ();
	pp_print_space ff ();
	pp_expr ff ();
	pp_print_char ff ';';
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
		pp_print_string ff label;
		pp_print_string ff " : "
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
	pp_print_string ff "end";
	if label <> "" then (
		pp_print_char ff ' ';
		pp_print_string ff label
	);
	pp_print_char ff ';'
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

let pp_character_literal (ff: formatter) (c: char): unit = (
	begin match c with
	| '\x00' ->
		pp_print_string ff "ASCII.NUL"
	| '\x01' ->
		pp_print_string ff "ASCII.SOH"
	| '\x02' ->
		pp_print_string ff "ASCII.STX"
	| '\x03' ->
		pp_print_string ff "ASCII.ETX"
	| '\x04' ->
		pp_print_string ff "ASCII.EOT"
	| '\x05' ->
		pp_print_string ff "ASCII.ENQ"
	| '\x06' ->
		pp_print_string ff "ASCII.ACK"
	| '\x07' ->
		pp_print_string ff "ASCII.BEL"
	| '\x08' ->
		pp_print_string ff "ASCII.BS"
	| '\x09' ->
		pp_print_string ff "ASCII.HT"
	| '\x0a' ->
		pp_print_string ff "ASCII.LF"
	| '\x0b' ->
		pp_print_string ff "ASCII.VT"
	| '\x0c' ->
		pp_print_string ff "ASCII.FF"
	| '\x0d' ->
		pp_print_string ff "ASCII.CR"
	| '\x0e' ->
		pp_print_string ff "ASCII.SO"
	| '\x0f' ->
		pp_print_string ff "ASCII.SI"
	| '\x10' ->
		pp_print_string ff "ASCII.DLE"
	| '\x11' ->
		pp_print_string ff "ASCII.DC1"
	| '\x12' ->
		pp_print_string ff "ASCII.DC2"
	| '\x13' ->
		pp_print_string ff "ASCII.DC3"
	| '\x14' ->
		pp_print_string ff "ASCII.DC4"
	| '\x15' ->
		pp_print_string ff "ASCII.NAK"
	| '\x16' ->
		pp_print_string ff "ASCII.SYN"
	| '\x17' ->
		pp_print_string ff "ASCII.ETB"
	| '\x18' ->
		pp_print_string ff "ASCII.CAN"
	| '\x19' ->
		pp_print_string ff "ASCII.EM"
	| '\x1a' ->
		pp_print_string ff "ASCII.SUB"
	| '\x1b' ->
		pp_print_string ff "ASCII.ESC"
	| '\x1c' ->
		pp_print_string ff "ASCII.FS"
	| '\x1d' ->
		pp_print_string ff "ASCII.GS"
	| '\x1e' ->
		pp_print_string ff "ASCII.RS"
	| '\x1f' ->
		pp_print_string ff "ASCII.US"
	| ' ' .. '~' ->
		pp_print_char ff '\'';
		pp_print_char ff c;
		pp_print_char ff '\''
	| '\x7f' ->
		pp_print_string ff "ASCII.DEL"
	| '\x80' .. '\xff' ->
		pp_print_string ff "Character'Val ";
		pp_open_paren ff ();
		pp_print_int ff (int_of_char c);
		pp_close_paren ff ()
	end
);;

let pp_string_literal
	(ff: formatter)
	?(width: int = 73)
	(pp_character_literal: formatter -> char -> unit)
	(first: int)
	(s: string)
	: unit =
(
	let printable c = (c >= ' ' && c <= '~') in
	let rec loop q w i = (
		if i >= String.length s then (
			if q then pp_print_char ff '\"'
		) else (
			let c = s.[i] in
			if not (printable c) then (
				if q then pp_print_char ff '\"';
				if i > 0 then (
					pp_print_space ff ();
					pp_print_string ff "& ";
				);
				pp_character_literal ff c;
				loop false 0 (i + 1)
			) else if w >= width - 1 then ( (* -1 means closing double-quote *)
				let w =
					if not q then 0 else (
						pp_print_char ff '\"';
						pp_print_space ff ();
						pp_print_string ff "& \"";
						3
					)
				in
				let w =
					begin match c with
					| '\"' ->
						pp_print_string ff "\"\"";
						w + 2
					| _ ->
						pp_print_char ff c;
						w + 1
					end
				in
				loop true w (i + 1)
			) else (
				let w =
					if q then (
						w
					) else if i > 0 then (
						pp_print_space ff ();
						pp_print_string ff "& \"";
						w + 3
					) else (
						pp_print_char ff '\"';
						w + 1
					)
				in
				let w =
					begin match c with
					| '\"' ->
						pp_print_string ff "\"\"";
						w + 2
					| _ ->
						pp_print_char ff c;
						w + 1
					end
				in
				loop true w (i + 1)
			)
		)
	) in
	if s = "" then (
		pp_print_string ff "\"\""
	) else if String.length s = 1 && (not (printable s.[0])) then (
		pp_print_char ff '(';
		pp_print_int ff first;
		pp_print_string ff " => ";
		pp_character_literal ff s.[0];
		pp_print_char ff ')'
	) else (
		loop false 0 0
	)
);;

let pp_array_literal
	(ff: formatter)
	(pp_component: formatter -> 'e -> unit)
	(first: int)
	(length: 'a -> int)
	(get: 'a -> int -> 'e)
	(item: 'a)
	: unit =
(
	let length = length item in
	if length = 0 then (
		pp_print_char ff '(';
		pp_print_int ff first;
		pp_print_string ff " .. ";
		pp_print_int ff (first - 1);
		pp_print_string ff " => <>)";
	) else if length = 1 then (
		pp_print_char ff '(';
		pp_print_int ff first;
		pp_print_string ff " => ";
		pp_component ff (get item 0);
		pp_print_char ff ')'
	) else (
		pp_print_char ff '(';
		pp_print_break ff 0 0;
		for i = 0 to length - 1 do
			if i > 0 then (
				pp_print_char ff ',';
				pp_print_space ff ()
			);
			pp_component ff (get item i);
		done;
		pp_print_char ff ')'
	)
);;

(* representation *)

let pp_for_type_storage_size
	(ff: formatter)
	(name: string)
	(size: int)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	pp_open_box ff indent;
	pp_print_string ff "for ";
	pp_print_string ff name;
	pp_print_char ff '\'';
	pp_print_break ff 0 0;
	pp_print_string ff "Storage_Size";
	pp_close_box ff ();
	pp_print_space ff ();
	pp_print_string ff "use ";
	pp_print_int ff size;
	pp_print_char ff ';';
	pp_close_box ff ()
);;

let pp_for_type_alignment
	(ff: formatter)
	(name: string)
	(pp_align: formatter -> 'a -> unit)
	(align: 'a)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	fprintf ff "for %s'Alignment@ use %a;" name pp_align align;
	pp_close_box ff ()
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
	fprintf ff "pragma No_Strict_Aliasing (@,%s);" name;
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

let pp_pragma_weak_external
	(ff: formatter)
	(name: string)
	: unit =
(
	pp_print_space ff ();
	pp_open_box ff indent;
	fprintf ff "pragma Weak_External (%s);" name;
	pp_close_box ff ()
);;
