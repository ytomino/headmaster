open Position;;
open Value;;

(* types *)

type int_prec = [
	| `signed_char
	| `unsigned_char
	| `signed_short
	| `unsigned_short
	| `signed_int
	| `unsigned_int
	| `signed_long
	| `unsigned_long
	| `signed_long_long
	| `unsigned_long_long];;

type float_prec = [
	| `float
	| `double
	| `long_double];;

type real_prec = [
	| float_prec
	| `decimal32 (* gcc's _Decimal32 *)
	| `decimal64 (* gcc's _Decimal64 *)
	| `decimal128];; (* gcc's _Decimal128 *)

(* operators in iso646.h *)

type operator = [
	| `ampersand
	| `and_assign
	| `and_then
	| `caret
	| `exclamation
	| `ne
	| `or_assign
	| `or_else
	| `tilde
	| `vertical
	| `xor_assign];;

(* for __attribute__((__mode__)) *)

type bit_width_mode = [
	| `__QI__ (* 8 *)
	| `__HI__ (* 16 *)
	| `__SI__ (* 32 *)
	| `__DI__ (* 64 *)
	| `__pointer__
	| `__unwind_word__ (* pointer size ? *)
	| `__word__];;

type builtin_comparator = [
	| `__builtin_isgreater
	| `__builtin_isgreaterequal
	| `__builtin_isless
	| `__builtin_islessequal
	| `__builtin_islessgreater
	| `__builtin_isunordered];;

module Syntax (Literals: LiteralsType) = struct
	open Literals;;
	
	type 'a p = ranged_position * 'a;;
	type 'a pe = [`some of ranged_position * 'a | `error];;
	type 'a opt = [`some of ranged_position * 'a | `none];;
	
	type identifier = [`ident of string];;
	
	(* pragma *)
	type pragma = [`sharp_PRAGMA] p * pragma_directive pe
	and pragma_directive = [
		| `gcc of [`GCC] p * gcc_directive pe
		| `instance of [`INSTANCE] p * specifier_qualifier_list pe * declarator pe
		| `pack of [`PACK] p * [`l_paren] pe * pack_argument opt * [`r_paren] pe
		| `language_mapping of [`FOR] p * string pe * language_mapping pe]
	and gcc_directive = [
		| `fenv
		| `poison of [`POISON] p * poison_identifier_list pe
		| `visibility of [`VISIBILITY] p * visibility_argument pe
		| `system_header]
	and poison_identifier_list = [
		| `nil of identifier
		| `cons of poison_identifier_list p * identifier p]
	and visibility_argument = [
		| `push of [`PUSH] p * [`l_paren] pe * [`DEFAULT] pe * [`r_paren] pe
		| `pop]
	and pack_argument = [
		| `push of [`PUSH] p * [`comma] pe * Integer.t pe
		| `pop
		| `set of Integer.t]
	and language_mapping = [
		| `type_mapping of [`TYPE] p * type_name pe * [`assign] pe * [`chars_literal of string] pe
		| `overload of [`OVERLOAD] p * specifier_qualifier_list pe * declarator pe
		| `includes of [`chars_literal of string] p * [`INCLUDE] pe * [`chars_literal of string] pe]
	(* attribute *)
	and attribute_list = [
		| `nil of attribute
		| `cons of attribute_list p * attribute p]
	and attribute = [`__attribute__] p * [`l_paren] pe * [`l_paren] pe * attribute_item_list pe * [`r_paren] pe * [`r_paren] pe
	and attribute_item_list = [
		| `nil of attribute_item
		| `cons of attribute_item_list p * [`comma] p * attribute_item pe]
	and attribute_item = [
		| `aligned of string p * ([`l_paren] p * Integer.t p * [`r_paren] p) opt
		| `alloc_size of string p * [`l_paren] pe * argument_expression_list pe * [`r_paren] pe
		| `always_inline of string
		| `cdecl of string
		| `const of string
		| `deprecated of string
		| `dllimport of string
		| `dllexport of string
		| `fastcall
		| `format of string p * [`l_paren] p * identifier p * [`comma] p * Integer.t p * [`comma] p * Integer.t p * [`r_paren] p
		| `format_arg of string p * [`l_paren] p * Integer.t p * [`r_paren] p
		| `inline of string
		| `malloc
		| `mode of string p * [`l_paren] p * bit_width_mode p * [`r_paren] p
		| `noinline
		| `nonnull of string p * [`l_paren] pe * argument_expression_list pe * [`r_paren] pe
		| `noreturn of string
		| `nothrow
		| `packed of string
		| `pure
		| `returns_twice
		| `selectany
		| `sentinel
		| `stdcall
		| `thiscall
		| `unavailable
		| `unused of string
		| `used
		| `warn_unused_result
		| `weak_import]
	(* inline assembler *)
	and inline_assembler = [`__asm | `__asm__] p * [`VOLATILE | `__volatile__] opt * [`l_paren] pe * [`chars_literal of string] pe * ia_out opt * [`r_paren] pe * [`semicolon] pe
	and ia_out = [`colon] p * ia_argument_list opt * ia_in opt
	and ia_in = [`colon] p * ia_argument_list opt * ia_destructive opt
	and ia_argument_list = [
		| `nil of ia_argument
		| `cons of ia_argument_list p * [`comma] p * ia_argument pe]
	and ia_argument = [`chars_literal of string] p * [`l_paren] pe * assignment_expression pe * [`r_paren] pe
	and ia_destructive = [`colon] p * ia_register_list pe
	and ia_register_list = [
		| `nil of [`chars_literal of string]
		| `cons of ia_register_list p * [`comma] p * [`chars_literal of string] pe]
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
		| `paren of [`l_paren] p * expression pe * [`r_paren] pe
		| `statement of [`l_paren] p * compound_statement p * [`r_paren] pe
		(* (6.5.2) postfix-expression *)
		| `array_access of expression p * [`l_bracket] p * expression pe * [`r_bracket] pe
		| `function_call of expression p * [`l_paren] p * argument_expression_list opt * [`r_paren] pe
		| `__builtin_constant_p of [`__builtin_constant_p] p * [`l_paren] pe * expression pe * [`r_paren] pe (* extended *)
		| `__builtin_va_arg of [`__builtin_va_arg] p * [`l_paren] pe * expression pe * [`comma] pe * type_name pe * [`r_paren] pe (* extended *)
		| `__builtin_compare of builtin_comparator p * [`l_paren] pe * expression pe * [`comma] pe * expression pe * [`r_paren] pe (* extended *)
		| `element_access of expression p * [`period] p * identifier pe
		| `dereferencing_element_access of expression p * [`arrow] p * identifier pe
		| `post_increment of expression p * [`increment] p
		| `post_decrement of expression p * [`decrement] p
		| `compound of [`l_paren] p * type_name p * [`r_paren] pe * [`l_curly] pe * initializer_list pe * [`r_curly] pe
		(* (6.5.3) unary-expression *)
		| `increment of [`increment] p * expression pe
		| `decrement of [`decrement] p * expression pe
		| `unary of unary_operator p * expression pe
		| `sizeof_expr of [`SIZEOF] p * expression pe
		| `sizeof_type of [`SIZEOF] p * [`l_paren] p * type_name p * [`r_paren] pe
		| `extension of [`__extension__] p * expression pe (* extended *)
		| `real of [`__real__] p * expression pe (* extended *)
		| `imag of [`__imag__] p * expression pe (* extended *)
		(* (6.5.4) cast-expression *)
		| `cast of [`l_paren] p * type_name p * [`r_paren] pe * expression pe
		(* (6.5.5) multiplicative-expression *)
		| `mul of expression p * [`asterisk] p * expression pe
		| `div of expression p * [`slash] p * expression pe
		| `rem of expression p * [`percent] p * expression pe
		(* (6.5.6) additive-expression *)
		| `add of expression p * [`plus] p * expression pe
		| `sub of expression p * [`minus] p * expression pe
		(* (6.5.7) shift-expression *)
		| `l_shift of expression p * [`l_shift] p * expression pe
		| `r_shift of expression p * [`r_shift] p * expression pe
		(* (6.5.8) relational-expression *)
		| `lt of expression p * [`lt] p * expression pe
		| `gt of expression p * [`gt] p * expression pe
		| `le of expression p * [`le] p * expression pe
		| `ge of expression p * [`ge] p * expression pe
		(* (6.5.9) equality-expression *)
		| `eq of expression p * [`eq] p * expression pe
		| `ne of expression p * [`ne] p * expression pe
		(* (6.5.10) AND-expression *)
		| `bit_and of expression p * [`ampersand] p * expression pe
		(* (6.5.11) exclusive-OR-expression *)
		| `bit_xor of expression p * [`caret] p * expression pe
		(* (6.5.12) inclusive-OR-expression *)
		| `bit_or of expression p * [`vertical] p * expression pe
		(* (6.5.13) logical-AND-expression *)
		| `and_then of expression p * [`and_then] p * expression pe
		(* (6.5.14) logical-OR-expression *)
		| `or_else of expression p * [`or_else] p * expression pe
		(* (6.5.15) conditional-expression *)
		| `cond of expression p * [`question] p * expression pe * [`colon] pe * expression pe
		(* (6.5.16) assignment-expression *)
		| `assign of expression p * assignment_operator p * expression pe
		(* (6.5.17) expression *)
		| `comma of expression p * [`comma] p * expression pe]
	and argument_expression_list = [
		(* (6.5.2) argument-expression-list *)
		| `nil of assignment_expression
		| `cons of argument_expression_list p * [`comma] p * assignment_expression pe]
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
		declaration_specifiers p * init_declarator_list opt * [`semicolon] pe
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
		| `cons of init_declarator_list p * [`comma] p * init_declarator pe]
	and init_declarator = [
		(* (6.7) init-declarator *)
		| `no_init of declarator
		| `with_init of declarator p * [`assign] p * initializer_t pe]
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
		| `with_body of struct_or_union p * identifier opt * [`l_curly] p * struct_declaration_list pe * [`r_curly] pe * attribute_list opt (* extended *)
		| `no_body of struct_or_union p * identifier pe]
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
		| `named of specifier_qualifier_list p * struct_declarator_list pe * [`semicolon] pe
		| `anonymous_struct_or_union of [`__extension__] p * struct_or_union_specifier pe * [`semicolon] pe] (* extended *)
	and specifier_qualifier_list = [
		(* (6.7.2.1) specifier-qualifier-list *)
		| `type_specifier of type_specifier p * specifier_qualifier_list opt
		| `type_qualifier of type_qualifier p * specifier_qualifier_list opt]
	and struct_declarator_list = [
		(* (6.7.2.1) struct-declarator-list *)
		| `nil of struct_declarator
		| `cons of struct_declarator_list p * [`comma] p * struct_declarator pe]
	and struct_declarator = [
		(* (6.7.2.1) struct-declarator *)
		| `item of declarator
		| `bit_width of declarator opt * [`colon] p * constant_expression pe]
	and enum_specifier = [
		(* (6.7.2.2) enum-specifier *)
		| `with_body of [`ENUM] p * identifier opt * [`l_curly] p * enumerator_list pe * [`comma] opt * [`r_curly] pe
		| `no_body of [`ENUM] p * identifier pe]
	and enumerator_list = [
		(* (6.7.2.2) enumerator-list *)
		| `nil of enumerator
		| `cons of enumerator_list p * [`comma] p * enumerator pe]
	and enumerator = [
		(* (6.7.2.2) enumerator *)
		| `no_repr of string
		| `with_repr of string p * [`assign] p * constant_expression pe]
	and type_qualifier = [
		(* (6.7.3) type-qualifier *)
		| `CONST
		| `RESTRICT
		| `VOLATILE
		| `__restrict__] (* extended *)
	and function_specifier = [
		(* (6.7.4) function-specifier *)
		| `INLINE
		| `__inline (* extended *)
		| `__inline__] (* extended *)
	and declarator =
		(* (6.7.5) declarator *)
		pointer opt * direct_declarator pe * attribute_list opt (* extended *)
	and direct_declarator = [
		(* (6.7.5) direct-declarator *)
		| `ident of string
		| `paren of [`l_paren] p * attribute_list opt * declarator pe * [`r_paren] pe (* extended *)
		| `array of direct_declarator p * [`l_bracket] p * type_qualifier_list opt * assignment_expression opt * [`r_bracket] pe
		| `static_array1 of direct_declarator p * [`l_bracket] p * [`STATIC] p * type_qualifier_list opt * assignment_expression p * [`r_bracket] pe
		| `static_array2 of direct_declarator p * [`l_bracket] p * type_qualifier_list p * [`STATIC] p * assignment_expression p * [`r_bracket] pe
		| `dynamic_array of direct_declarator p * [`l_bracket] p * type_qualifier_list opt * [`asterisk] p * [`r_bracket] pe
		| `function_type of direct_declarator p * [`l_paren] p * parameter_type_list p * [`r_paren] pe
		| `old_function_type of direct_declarator p * [`l_paren] p * identifier_list opt * [`r_paren] pe]
	and pointer = [
		(* (6.7.5) pointer *)
		| `nil of [`asterisk] p * type_qualifier_list opt * attribute_list opt (* extended *)
		| `cons of [`asterisk] p * type_qualifier_list opt * pointer p]
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
		| `cons of parameter_list p * [`comma] p * parameter_declaration pe]
	and parameter_declaration = [
		(* (6.7.5) parameter-declaration *)
		| `with_name of declaration_specifiers p * declarator p
		| `no_name of declaration_specifiers p * abstract_declarator opt]
	and identifier_list = [
		(* (6.7.5) identifier-list *)
		| `nil of identifier
		| `cons of identifier_list p * [`comma] p * identifier pe]
	and type_name =
		(* (6.7.6) type-name *)
		specifier_qualifier_list p * abstract_declarator opt
	and abstract_declarator = [
		(* (6.7.6) abstract-declarator *)
		| `pointer of pointer
		| `declarator of pointer opt * direct_abstract_declarator p]
	and direct_abstract_declarator = [
		(* (6.7.6) direct-abstract-declarator *)
		| `paren of [`l_paren] p * attribute_list opt * abstract_declarator pe * [`r_paren] pe
		| `array of direct_abstract_declarator opt * [`l_bracket] p * type_qualifier_list opt * assignment_expression opt * [`r_bracket] pe
		| `static_array1 of direct_abstract_declarator opt * [`l_bracket] p * [`STATIC] p * type_qualifier_list opt * assignment_expression p * [`r_bracket] pe
		| `static_array2 of direct_abstract_declarator opt * [`l_bracket] p * type_qualifier_list p * [`STATIC] p * assignment_expression p * [`r_bracket] pe
		| `dynamic_array of direct_abstract_declarator opt * [`l_bracket] p * [`asterisk] p * [`r_bracket] pe
		| `function_type of direct_abstract_declarator opt * [`l_paren] p * parameter_type_list opt * [`r_paren] pe]
	and typedef_name =
		(* (6.7.7) typedef-name *)
		identifier
	and initializer_t = [
		(* (6.7.8) initializer *)
		| `expression of assignment_expression
		| `list of [`l_curly] p * initializer_list pe * [`r_curly] pe]
	and initializer_list = [
		(* (6.7.8) initializer-list *)
		| `nil of designation opt * initializer_t pe
		| `cons of initializer_list p * [`comma] p * designation opt * initializer_t pe]
	and designation =
		(* (6.7.8) designation *)
		designator_list p * [`assign] pe
	and designator_list = [
		(* (6.7.8) designator-list *)
		| `nil of designator
		| `cons of designator_list p * designator p]
	and designator = [
		(* (6.7.8) designator *)
		| `index of [`l_bracket] p * constant_expression pe * [`r_bracket] pe
		| `element of [`period] p * identifier pe]
	(* A.2.3 Statements *)
	and statement = [
		(* (6.8) statement *)
		| `asm of inline_assembler (* extended *)
		(* (6.8.1) labeled-statement *)
		| `label of identifier p * [`colon] p * statement pe
		| `case of [`CASE] p * constant_expression pe * [`colon] pe * statement pe
		| `default of [`DEFAULT] p * [`colon] pe * statement pe
		(* (6.8.2) compound-statement *)
		| `compound of compound_statement
		(* (6.8.3) expression-statement *)
		| `expression of expression opt * [`semicolon] pe
		| `__builtin_va_start of [`__builtin_va_start] p * [`l_paren] pe * expression pe * [`comma] pe * expression pe * [`r_paren] pe * [`semicolon] pe (* extended *)
		| `__builtin_va_end of [`__builtin_va_end] p * [`l_paren] pe * expression pe * [`r_paren] pe * [`semicolon] pe (* extended *)
		| `__builtin_va_copy of [`__builtin_va_copy] p * [`l_paren] pe * expression pe * [`comma] pe * expression pe * [`r_paren] pe * [`semicolon] pe (* extended *)
		(* (6.8.4) selection-statement *)
		| `if_then of [`IF] p * [`l_paren] pe * expression pe * [`r_paren] pe * statement pe
		| `if_then_else of [`IF] p * [`l_paren] pe * expression pe * [`r_paren] pe * statement pe * [`ELSE] p * statement pe
		| `switch of [`SWITCH] p * [`l_paren] pe * expression pe * [`r_paren] pe * statement pe
		(* (6.8.5) iteration-statement *)
		| `while_loop of [`WHILE] p * [`l_paren] pe * expression pe * [`r_paren] pe * statement pe
		| `do_loop of [`DO] p * statement pe * [`WHILE] pe * [`l_paren] pe * expression pe * [`r_paren] pe * [`semicolon] pe
		| `for_loop of [`FOR] p * [`l_paren] pe * expression opt * [`semicolon] pe * expression opt * [`semicolon] pe * expression opt * [`r_paren] pe * statement pe
		| `for_with_declaration of [`FOR] p * [`l_paren] pe * declaration p * expression opt * [`semicolon] pe * expression opt * [`r_paren] pe * statement pe
		(* (6.8.6) jump-statement *)
		| `goto of [`GOTO] p * identifier pe * [`semicolon] pe
		| `continue of [`CONTINUE] p * [`semicolon] pe
		| `break of [`BREAK] p * [`semicolon] pe
		| `return of [`RETURN] p * expression opt * [`semicolon] pe]
	and compound_statement =
		(* (6.8.2) compound-statement *)
		[`l_curly] p * block_item_list opt * [`r_curly] pe
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
		declaration_specifiers p * declarator pe * declaration_list opt * compound_statement pe
	and aliased_declaration =
		(* extended *)
		declaration_specifiers p * declarator pe * [`__asm] p * [`l_paren] pe * [`chars_literal of string] pe * [`r_paren] pe * attribute_list opt * [`semicolon] pe
	and declaration_list = [
		(* (6.9.1) declaration-list *)
		| `nil of declaration
		| `cons of declaration_list p * declaration p];;
	
end;;

module SyntaxType (Literals: LiteralsType) = struct
	module type S = module type of Syntax (Literals);;
end;;
