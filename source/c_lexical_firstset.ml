open C_literals;;

module FirstSet
	(Literals: LiteralsType) =
struct
	open Literals;;
	
	type identifier = [`ident of string]
	and typedef_name = identifier;;
	(* some sets including typedef_name in reality are marked by quote *)
	
	(* A.2.1 Expressions *)
	
	type unary_operator = [
		| `ampersand
		| `asterisk
		| `plus
		| `minus
		| `tilde
		| `exclamation];;
	type assignment_operator = [
		| `assign
		| `mul_assign
		| `div_assign
		| `rem_assign
		| `add_assign
		| `sub_assign
		| `l_shift_assign
		| `r_shift_assign
		| `and_assign
		| `xor_assign
		| `or_assign];;
	
	type firstset_of_primary_expression = [
		| identifier
		| `numeric_literal of string * [
			| `int_literal of int_prec * Integer.t
			| `float_literal of real_prec * Real.t
			| `imaginary_literal of float_prec * Real.t] (* extended *)
		| `char_literal of char
		| `chars_literal of string
		| `wchar_literal of WideString.elm
		| `wchars_literal of WideString.t
		| `objc_string_literal of string (* @"..." *)
		| `__FILE__
		| `__LINE__
		| `l_paren];;
	type firstset_of_postfix_expression = [
		| firstset_of_primary_expression
		| `__builtin_va_arg];;
	type firstset_of_unary_op_expression = [
		| firstset_of_postfix_expression
		| unary_operator
		| `increment
		| `decrement
		| `SIZEOF
		| `__extension__
		| `__real__
		| `__imag__];;
	type firstset_of_cast_expression = firstset_of_unary_op_expression;;
	type firstset_of_binary_op_expression = firstset_of_cast_expression;;
	type firstset_of_conditional_expression = firstset_of_binary_op_expression;;
	type firstset_of_assignment_expression = firstset_of_conditional_expression;;
	type firstset_of_expression = firstset_of_assignment_expression;;
	type firstset_of_constant_expression = firstset_of_conditional_expression;;
	type firstset_of_initializer = [
		| firstset_of_assignment_expression
		| `l_curly];;
	
	(* A.2.2 Declarations *)
	
	type storage_class_specifier = [
		| `TYPEDEF
		| `EXTERN
		| `STATIC
		| `AUTO
		| `REGISTER];;
	type simple_type_specifier = [
		| `VOID
		| `CHAR
		| `SHORT
		| `INT
		| `LONG
		| `FLOAT
		| `DOUBLE
		| `SIGNED
		| `UNSIGNED
		| `_BOOL
		| `_IMAGINARY
		| `_COMPLEX
		| `__int64
		| `__builtin_va_list];;
	type struct_or_union = [
		| `STRUCT
		| `UNION];;
	type type_qualifier = [
		| `CONST
		| `__const
		| `RESTRICT
		| `VOLATILE
		| `__restrict
		| `__restrict__];;
	type function_specifier = [
		| `INLINE
		| `__inline
		| `__inline__];;
	
	type firstset_of_type_specifier' = [
		| simple_type_specifier
		| struct_or_union
		| `ENUM];;
	type firstset_of_type_name' = [
		| firstset_of_type_specifier'
		| type_qualifier];;
	type firstset_of_direct_declarator = [
		| identifier
		| `l_paren];;
	type firstset_of_pointer = [
		| `asterisk
		| `caret];;
	type firstset_of_declarator = [
		| firstset_of_direct_declarator
		| firstset_of_pointer];;
	type firstset_of_direct_abstract_declarator = [
		| `l_paren
		| `l_bracket];;
	type firstset_of_abstract_declarator = [
		| firstset_of_direct_abstract_declarator
		| firstset_of_pointer];;
	type firstset_of_struct_declaration' = [
		| firstset_of_type_specifier'
		| type_qualifier
		| `__extension__];;
	type firstset_of_specifier_qualifier_list' = [
		| firstset_of_type_specifier'
		| type_qualifier];;
	type firstset_of_struct_declarator = [
		| firstset_of_declarator
		| `colon];;
	type firstset_of_declaration_specifiers' = [
		| storage_class_specifier
		| firstset_of_type_specifier'
		| type_qualifier
		| function_specifier
		| `__attribute__
		| `__extension__];;
	type firstset_of_designator = [
		| `l_bracket
		| `period];;
	
	(* A.2.3 Statements *)
	
	type asm = [
		| `__asm
		| `__asm__];;
	
	type firstset_of_statement = [
		| firstset_of_expression
		| asm
		| `l_curly
		| `semicolon
		| `BREAK
		| `CASE
		| `CONTINUE
		| `DEFAULT
		| `DO
		| `FOR
		| `GOTO
		| `IF
		| `RETURN
		| `SWITCH
		| `WHILE
		| `__builtin_va_start
		| `__builtin_va_end
		| `__builtin_va_copy];;
	type firstset_of_block_item = [
		| firstset_of_declaration_specifiers'
		| firstset_of_statement];;
	
end;;
