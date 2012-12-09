open C_literals;;
open C_syntax;;

module FirstSet
	(Literals: LiteralsType)
	(Syntax: SyntaxType (Literals).S) =
struct
	open Literals;;
	open Syntax;;
	
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
	
	type firstset_of_type_specifier = [
		| simple_type_specifier
		| `STRUCT
		| `UNION
		| `ENUM];;
	type firstset_of_type_name = [
		| firstset_of_type_specifier
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
	type firstset_of_struct_declaration = [
		| firstset_of_type_specifier
		| type_qualifier
		| `__extension__];;
	type firstset_of_declaration_specifiers = [
		| storage_class_specifier
		| firstset_of_type_specifier
		| type_qualifier
		| function_specifier
		| `__attribute__
		| `__extension__];;
	type firstset_of_designator = [
		| `l_bracket
		| `period];;
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
	type firstset_of_asm = [
		| `__asm
		| `__asm__];;
	type firstset_of_statement = [
		| firstset_of_expression
		| firstset_of_asm
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
		| firstset_of_declaration_specifiers
		| firstset_of_statement];;
	
end;;
