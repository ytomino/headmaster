open C_literals;;
open Position;;

module Syntax (Literals: LiteralsType) = struct
	open Literals;;
	
	type 'a p = ranged_position * 'a;;
	type 'a e = [`some of ranged_position * 'a | `error];; (* expected *)
	type 'a opt = [`some of ranged_position * 'a | `none];;
	
	type identifier = [`ident of string];;
	
	(* pragma *)
	type pragma = [`sharp_PRAGMA] p * pragma_directive e
	and pragma_directive = [
		| `gcc of [`GCC] p * gcc_directive e
		| `instance of [`INSTANCE] p * specifier_qualifier_list e * declarator e
		| `pack of [`PACK] p * [`l_paren] e * pack_argument opt * [`r_paren] e
		| `language_mapping of [`FOR] p * string e * language_mapping e]
	and gcc_directive = [
		| `fenv
		| `poison of [`POISON] p * poison_identifier_list e
		| `visibility of [`VISIBILITY] p * visibility_argument e
		| `system_header]
	and poison_identifier_list = [
		| `nil of identifier
		| `cons of poison_identifier_list p * identifier p]
	and visibility_argument = [
		| `push of [`PUSH] p * [`l_paren] e * [`DEFAULT] e * [`r_paren] e
		| `pop]
	and pack_argument = [
		| `push of [`PUSH] p * [`comma] e * Integer.t e
		| `pop
		| `set of Integer.t]
	and language_mapping = [
		| `type_mapping of [`TYPE] p * type_name e * [`assign] e * [`chars_literal of string] e
		| `overload of [`OVERLOAD] p * specifier_qualifier_list e * declarator e
		| `includes of [`chars_literal of string] p * [`INCLUDE] e * [`chars_literal of string] e
		| `monolithic_include of [`chars_literal of string] p * [`MONOLITHIC_INCLUDE] e * [`chars_literal of string] e]
	(* attribute *)
	and attribute_list = [
		| `nil of attribute
		| `cons of attribute_list p * attribute p]
	and attribute = [`__attribute__] p * [`l_paren] e * [`l_paren] e * attribute_item_list e * [`r_paren] e * [`r_paren] e
	and attribute_item_list = [
		| `nil of attribute_item
		| `cons of attribute_item_list p * [`comma] p * attribute_item e]
	and attribute_item = [
		| `aligned of string p * ([`l_paren] p * assignment_expression e * [`r_paren] e) opt
		| `alloc_size of string p * [`l_paren] e * argument_expression_list e * [`r_paren] e
		| `always_inline of string
		| `artificial
		| `blocks of string p * [`l_paren] e * [`BYREF] e * [`r_paren] e
		| `cdecl of string
		| `const of string
		| `deprecated of string
		| `dllimport of string
		| `dllexport of string
		| `fastcall
		| `format of string p * [`l_paren] e * identifier e * [`comma] e * assignment_expression e * [`comma] e * assignment_expression e * [`r_paren] e
		| `format_arg of string p * [`l_paren] e * assignment_expression e * [`r_paren] e
		| `inline of string
		| `leaf
		| `malloc
		| `mode of string p * [`l_paren] e * bit_width_mode e * [`r_paren] e
		| `noinline of string
		| `nonnull of string p * [`l_paren] e * argument_expression_list e * [`r_paren] e
		| `noreturn of string
		| `nothrow
		| `objc_gc of string p * [`l_paren] e * [`WEAK] e * [`r_paren] e
		| `optimize of string p * [`l_paren] e * [`chars_literal of string] e * [`r_paren] e
		| `packed of string
		| `pure
		| `regparm of string p * [`l_paren] e * assignment_expression e * [`r_paren] e
		| `returns_twice
		| `selectany
		| `sentinel
		| `stdcall
		| `thiscall
		| `transparent_union
		| `unavailable
		| `unused of string
		| `used
		| `visibility of string p * [`l_paren] e * [`chars_literal of string] e * [`r_paren] e
		| `warn_unused_result
		| `weak
		| `weak_import]
	(* inline assembler *)
	and inline_assembler = [`__asm | `__asm__] p * [`VOLATILE | `__volatile__] opt * [`l_paren] e * [`chars_literal of string] e * ia_out opt * [`r_paren] e * [`semicolon] e
	and ia_out = [`colon] p * ia_argument_list opt * ia_in opt
	and ia_in = [`colon] p * ia_argument_list opt * ia_destructive opt
	and ia_argument_list = [
		| `nil of ia_argument
		| `cons of ia_argument_list p * [`comma] p * ia_argument e]
	and ia_argument = [`chars_literal of string] p * [`l_paren] e * assignment_expression e * [`r_paren] e
	and ia_destructive = [`colon] p * ia_register_list e
	and ia_register_list = [
		| `nil of [`chars_literal of string]
		| `cons of ia_register_list p * [`comma] p * [`chars_literal of string] e]
	(* A.2.1 Expressions *)
	and expression = [
		(* (6.5.1) primary-expression *)
		| `ident of string
		| `int_literal of int_prec * Integer.t
		| `float_literal of real_prec * Real.t
		| `imaginary_literal of float_prec * Real.t (* extended *)
		| `char_literal of char
		| `chars_literal of string
		| `wchar_literal of WideString.elm
		| `wchars_literal of WideString.t
		| `objc_string_literal of string (* @"..." *)
		| `__FILE__ of string
		| `__LINE__ of Integer.t
		| `paren of [`l_paren] p * expression e * [`r_paren] e
		| `statement of [`l_paren] p * compound_statement p * [`r_paren] e
		(* (6.5.2) postfix-expression *)
		| `array_access of expression p * [`l_bracket] p * expression e * [`r_bracket] e
		| `function_call of expression p * [`l_paren] p * argument_expression_list opt * [`r_paren] e
		| `__builtin_constant_p of [`__builtin_constant_p] p * [`l_paren] e * expression e * [`r_paren] e (* extended *)
		| `__builtin_expect of [`__builtin_expect] p * [`l_paren] e * expression e * [`comma] e * expression e * [`r_paren] e (* extended *)
		| `__builtin_object_size of [`__builtin_object_size] p * [`l_paren] e * expression e * [`comma] e * expression e * [`r_paren] e (* extended *)
		| `__builtin_va_arg of [`__builtin_va_arg] p * [`l_paren] e * expression e * [`comma] e * type_name e * [`r_paren] e (* extended *)
		| `__builtin_compare of builtin_comparator p * [`l_paren] e * expression e * [`comma] e * expression e * [`r_paren] e (* extended *)
		| `element_access of expression p * [`period] p * identifier e
		| `dereferencing_element_access of expression p * [`arrow] p * identifier e
		| `post_increment of expression p * [`increment] p
		| `post_decrement of expression p * [`decrement] p
		| `compound of [`l_paren] p * type_name p * [`r_paren] e * [`l_curly] e * initializer_list e * [`r_curly] e
		(* (6.5.3) unary-expression *)
		| `increment of [`increment] p * expression e
		| `decrement of [`decrement] p * expression e
		| `unary of unary_operator p * expression e
		| `sizeof_expr of [`SIZEOF] p * expression e
		| `sizeof_type of [`SIZEOF] p * [`l_paren] p * type_name p * [`r_paren] e
		| `extension of [`__extension__] p * expression e (* extended *)
		| `real of [`__real__] p * expression e (* extended *)
		| `imag of [`__imag__] p * expression e (* extended *)
		(* (6.5.4) cast-expression *)
		| `cast of [`l_paren] p * type_name p * [`r_paren] e * expression e
		(* (6.5.5) multiplicative-expression *)
		| `mul of expression p * [`asterisk] p * expression e
		| `div of expression p * [`slash] p * expression e
		| `rem of expression p * [`percent] p * expression e
		(* (6.5.6) additive-expression *)
		| `add of expression p * [`plus] p * expression e
		| `sub of expression p * [`minus] p * expression e
		(* (6.5.7) shift-expression *)
		| `l_shift of expression p * [`l_shift] p * expression e
		| `r_shift of expression p * [`r_shift] p * expression e
		(* (6.5.8) relational-expression *)
		| `lt of expression p * [`lt] p * expression e
		| `gt of expression p * [`gt] p * expression e
		| `le of expression p * [`le] p * expression e
		| `ge of expression p * [`ge] p * expression e
		(* (6.5.9) equality-expression *)
		| `eq of expression p * [`eq] p * expression e
		| `ne of expression p * [`ne] p * expression e
		(* (6.5.10) AND-expression *)
		| `bit_and of expression p * [`ampersand] p * expression e
		(* (6.5.11) exclusive-OR-expression *)
		| `bit_xor of expression p * [`caret] p * expression e
		(* (6.5.12) inclusive-OR-expression *)
		| `bit_or of expression p * [`vertical] p * expression e
		(* (6.5.13) logical-AND-expression *)
		| `and_then of expression p * [`and_then] p * expression e
		(* (6.5.14) logical-OR-expression *)
		| `or_else of expression p * [`or_else] p * expression e
		(* (6.5.15) conditional-expression *)
		| `cond of expression p * [`question] p * expression e * [`colon] e * expression e
		(* (6.5.16) assignment-expression *)
		| `assign of expression p * assignment_operator p * expression e
		(* (6.5.17) expression *)
		| `comma of expression p * [`comma] p * expression e]
	and argument_expression_list = [
		(* (6.5.2) argument-expression-list *)
		| `nil of assignment_expression
		| `cons of argument_expression_list p * [`comma] p * assignment_expression e]
	and builtin_comparator = [
		(* extended *)
		| `__builtin_isgreater
		| `__builtin_isgreaterequal
		| `__builtin_isless
		| `__builtin_islessequal
		| `__builtin_islessgreater
		| `__builtin_isunordered]
	and unary_operator = [
		(* (6.5.3) unary-operator *)
		| `ampersand
		| `asterisk
		| `plus
		| `minus
		| `tilde
		| `exclamation]
	and assignment_operator = [
		(* (6.5.16) assignment-operator *)
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
		| `or_assign]
	and assignment_expression =
		(* (6.5.16) assignment-expression *)
		expression
	and constant_expression =
		(* (6.6) constant-expression *)
		expression
	(* A.2.2 Declarations *)
	and declaration =
		(* (6.7) declaration *)
		declaration_specifiers p * init_declarator_list opt * [`semicolon] e
	and declaration_specifiers = [
		(* (6.7) declaration-specifiers *)
		| `storage_class_specifier of storage_class_specifier p * declaration_specifiers opt
		| `type_specifier of type_specifier p * declaration_specifiers opt
		| `type_qualifier of type_qualifier p * declaration_specifiers opt
		| `function_specifier of function_specifier p * declaration_specifiers opt
		| `attributes of attribute p * declaration_specifiers opt (* extended *)
		| `extension of [`__extension__] p * declaration_specifiers opt] (* extended *)
	and init_declarator_list = [
		(* (6.7) init-declarator-list *)
		| `nil of init_declarator
		| `cons of init_declarator_list p * [`comma] p * init_declarator e]
	and init_declarator = [
		(* (6.7) init-declarator *)
		| `no_init of declarator
		| `with_init of declarator p * [`assign] p * initializer_t e]
	and storage_class_specifier = [
		(* (6.7.1) storage-class-specifier *)
		| `TYPEDEF
		| `EXTERN
		| `STATIC
		| `AUTO
		| `REGISTER]
	and type_specifier = [
		(* (6.7.2) type-specifier *)
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
		| `struct_or_union_specifier of struct_or_union_specifier
		| `enum_specifier of enum_specifier
		| `typedef_name of typedef_name
		| `__int64 (* extended *)
		| `__builtin_va_list] (* extended *)
	and struct_or_union_specifier = [
		(* (6.7.2.1) struct-or-union-specifier *)
		| `with_body of struct_or_union p * attribute_list opt * identifier opt * [`l_curly] p * struct_declaration_list e * [`r_curly] e * attribute_list opt
		| `no_body of struct_or_union p * identifier e]
	and struct_or_union = [
		(* (6.7.2.1) struct-or-union *)
		| `STRUCT
		| `UNION]
	and struct_declaration_list = [
		(* (6.7.2.1) struct-declaration-list *)
		| `nil of struct_declaration
		| `cons of struct_declaration_list p * struct_declaration p]
	and struct_declaration = [
		(* (6.7.2.1) struct-declaration *)
		| `named of [`__extension__] opt * specifier_qualifier_list e * struct_declarator_list e * [`semicolon] e
		| `anonymous_struct_or_union of [`__extension__] opt * struct_or_union_specifier p * [`semicolon] e]
	and specifier_qualifier_list = [
		(* (6.7.2.1) specifier-qualifier-list *)
		| `type_specifier of type_specifier p * specifier_qualifier_list opt
		| `type_qualifier of type_qualifier p * specifier_qualifier_list opt]
	and struct_declarator_list = [
		(* (6.7.2.1) struct-declarator-list *)
		| `nil of struct_declarator
		| `cons of struct_declarator_list p * [`comma] p * struct_declarator e]
	and struct_declarator = [
		(* (6.7.2.1) struct-declarator *)
		| `item of declarator
		| `bit_width of declarator opt * [`colon] p * constant_expression e]
	and enum_specifier = [
		(* (6.7.2.2) enum-specifier *)
		| `with_body of [`ENUM] p * identifier opt * [`l_curly] p * enumerator_list e * [`comma] opt * [`r_curly] e
		| `no_body of [`ENUM] p * identifier e]
	and enumerator_list = [
		(* (6.7.2.2) enumerator-list *)
		| `nil of enumerator
		| `cons of enumerator_list p * [`comma] p * enumerator e]
	and enumerator = [
		(* (6.7.2.2) enumerator *)
		| `no_repr of string
		| `with_repr of string p * [`assign] p * constant_expression e]
	and type_qualifier = [
		(* (6.7.3) type-qualifier *)
		| `CONST
		| `__const (* extended *)
		| `RESTRICT
		| `VOLATILE
		| `__restrict (* extended *)
		| `__restrict__] (* extended *)
	and function_specifier = [
		(* (6.7.4) function-specifier *)
		| `INLINE
		| `__inline (* extended *)
		| `__inline__] (* extended *)
	and declarator =
		(* (6.7.5) declarator *)
		pointer opt * direct_declarator e * attribute_list opt
	and direct_declarator = [
		(* (6.7.5) direct-declarator *)
		| `ident of string
		| `paren of [`l_paren] p * attribute_list opt * declarator e * [`r_paren] e
		| `array of direct_declarator p * [`l_bracket] p * type_qualifier_list opt * assignment_expression opt * [`r_bracket] e
		| `static_array1 of direct_declarator p * [`l_bracket] p * [`STATIC] p * type_qualifier_list opt * assignment_expression p * [`r_bracket] e
		| `static_array2 of direct_declarator p * [`l_bracket] p * type_qualifier_list p * [`STATIC] p * assignment_expression p * [`r_bracket] e
		| `dynamic_array of direct_declarator p * [`l_bracket] p * type_qualifier_list opt * [`asterisk] p * [`r_bracket] e
		| `function_type of direct_declarator p * [`l_paren] p * parameter_type_list p * [`r_paren] e
		| `old_function_type of direct_declarator p * [`l_paren] p * identifier_list opt * [`r_paren] e]
	and pointer = [
		(* (6.7.5) pointer (caret pointer is extended) *)
		| `nil of [`asterisk | `caret] p * type_qualifier_list opt * attribute_list opt
		| `cons of [`asterisk | `caret] p * type_qualifier_list opt * pointer p]
	and type_qualifier_list = [
		(* (6.7.5) type-qualifier-list *)
		| `nil of type_qualifier
		| `cons of type_qualifier_list p * type_qualifier p]
	and parameter_type_list = [
		(* (6.7.5) parameter-type-list *)
		| `args of parameter_list
		| `varargs of parameter_list p * [`comma] p * [`varargs] p]
	and parameter_list = [
		(* (6.7.5) parameter-list *)
		| `nil of parameter_declaration
		| `cons of parameter_list p * [`comma] p * parameter_declaration e]
	and parameter_declaration = [
		(* (6.7.5) parameter-declaration *)
		| `with_name of declaration_specifiers p * declarator p
		| `no_name of declaration_specifiers p * abstract_declarator opt]
	and identifier_list = [
		(* (6.7.5) identifier-list *)
		| `nil of identifier
		| `cons of identifier_list p * [`comma] p * identifier e]
	and type_name =
		(* (6.7.6) type-name *)
		specifier_qualifier_list p * abstract_declarator opt
	and abstract_declarator = [
		(* (6.7.6) abstract-declarator *)
		| `pointer of pointer
		| `declarator of pointer opt * direct_abstract_declarator p]
	and direct_abstract_declarator = [
		(* (6.7.6) direct-abstract-declarator *)
		| `paren of [`l_paren] p * attribute_list opt * abstract_declarator e * [`r_paren] e
		| `array of direct_abstract_declarator opt * [`l_bracket] p * type_qualifier_list opt * assignment_expression opt * [`r_bracket] e
		| `static_array1 of direct_abstract_declarator opt * [`l_bracket] p * [`STATIC] p * type_qualifier_list opt * assignment_expression p * [`r_bracket] e
		| `static_array2 of direct_abstract_declarator opt * [`l_bracket] p * type_qualifier_list p * [`STATIC] p * assignment_expression p * [`r_bracket] e
		| `dynamic_array of direct_abstract_declarator opt * [`l_bracket] p * [`asterisk] p * [`r_bracket] e
		| `function_type of direct_abstract_declarator opt * [`l_paren] p * parameter_type_list opt * [`r_paren] e]
	and typedef_name =
		(* (6.7.7) typedef-name *)
		identifier
	and initializer_t = [
		(* (6.7.8) initializer *)
		| `expression of assignment_expression
		| `list of [`l_curly] p * initializer_list e * [`r_curly] e]
	and initializer_list = [
		(* (6.7.8) initializer-list *)
		| `nil of designation opt * initializer_t e
		| `cons of initializer_list p * [`comma] p * designation opt * initializer_t e]
	and designation =
		(* (6.7.8) designation *)
		designator_list p * [`assign] e
	and designator_list = [
		(* (6.7.8) designator-list *)
		| `nil of designator
		| `cons of designator_list p * designator p]
	and designator = [
		(* (6.7.8) designator *)
		| `index of [`l_bracket] p * constant_expression e * [`r_bracket] e
		| `element of [`period] p * identifier e]
	(* A.2.3 Statements *)
	and statement = [
		(* (6.8) statement *)
		| `asm of inline_assembler (* extended *)
		(* (6.8.1) labeled-statement *)
		| `label of identifier p * [`colon] p * statement e
		| `case of [`CASE] p * constant_expression e * [`colon] e * statement e
		| `default of [`DEFAULT] p * [`colon] e * statement e
		(* (6.8.2) compound-statement *)
		| `compound of compound_statement
		(* (6.8.3) expression-statement *)
		| `expression of expression opt * [`semicolon] e
		| `__builtin_va_start of [`__builtin_va_start] p * [`l_paren] e * expression e * [`comma] e * expression e * [`r_paren] e * [`semicolon] e (* extended *)
		| `__builtin_va_end of [`__builtin_va_end] p * [`l_paren] e * expression e * [`r_paren] e * [`semicolon] e (* extended *)
		| `__builtin_va_copy of [`__builtin_va_copy] p * [`l_paren] e * expression e * [`comma] e * expression e * [`r_paren] e * [`semicolon] e (* extended *)
		(* (6.8.4) selection-statement *)
		| `if_then of [`IF] p * [`l_paren] e * expression e * [`r_paren] e * statement e
		| `if_then_else of [`IF] p * [`l_paren] e * expression e * [`r_paren] e * statement e * [`ELSE] p * statement e
		| `switch of [`SWITCH] p * [`l_paren] e * expression e * [`r_paren] e * statement e
		(* (6.8.5) iteration-statement *)
		| `while_loop of [`WHILE] p * [`l_paren] e * expression e * [`r_paren] e * statement e
		| `do_loop of [`DO] p * statement e * [`WHILE] e * [`l_paren] e * expression e * [`r_paren] e * [`semicolon] e
		| `for_loop of [`FOR] p * [`l_paren] e * expression opt * [`semicolon] e * expression opt * [`semicolon] e * expression opt * [`r_paren] e * statement e
		| `for_with_declaration of [`FOR] p * [`l_paren] e * declaration p * expression opt * [`semicolon] e * expression opt * [`r_paren] e * statement e
		(* (6.8.6) jump-statement *)
		| `goto of [`GOTO] p * identifier e * [`semicolon] e
		| `continue of [`CONTINUE] p * [`semicolon] e
		| `break of [`BREAK] p * [`semicolon] e
		| `return of [`RETURN] p * expression opt * [`semicolon] e]
	and compound_statement =
		(* (6.8.2) compound-statement *)
		[`l_curly] p * block_item_list opt * [`r_curly] e
	and block_item_list = [
		(* (6.8.2) block-item-list *)
		| `nil of block_item
		| `cons of block_item_list p * block_item p]
	and block_item = [
		(* (6.8.2) block-item *)
		| `declaration of declaration
		| `statement of statement];;
	
	(* A.2.4 External definitions *)
	type translation_unit = [
		(* (6.9) translation-unit *)
		| `nil (* accepting an empty file *)
		| `cons of translation_unit * external_declaration p]
	and external_declaration = [
		(* (6.9) external-declaration *)
		| `function_definition of function_definition
		| `aliased_declaration of aliased_declaration (* extended *)
		| `declaration of declaration
		| `pragma of pragma] (* extended *)
	and function_definition =
		(* (6.9.1) function-definition *)
		declaration_specifiers p * declarator e * declaration_list opt * compound_statement e
	and aliased_declaration =
		(* extended *)
		declaration_specifiers p * declarator e * [`__asm | `__asm__] p * [`l_paren] e * [`chars_literal of string] e * [`r_paren] e * attribute_list opt * [`semicolon] e
	and declaration_list = [
		(* (6.9.1) declaration-list *)
		| `nil of declaration
		| `cons of declaration_list p * declaration p];;
	
end;;

module type SyntaxType = sig
	module Literals: LiteralsType;;
	include module type of Syntax (Literals);;
end;;
