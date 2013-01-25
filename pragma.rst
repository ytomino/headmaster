pragma for-include
------------------

Renaming symbols in one module into other module.

Syntax::

	#pragma for TARGET-LANG "FILE1" include "FILE2"

Example::

	#pragma for Ada "errno.h" include "sys/errno.h"

pragma for-monolithic_include
-----------------------------

Similar to ``for-include``, but ``FILE1`` assimilates ``FILE2`` completely.
This pragma is the last resort to resolve some kind of circular dependency.

Syntax::

	#pragma for TARGET-LANG "FILE1" monolithic_include "FILE2"

Example::

	#pragma for Ada "errno.h" monolithic_include "sys/errno.h"

pragma for-overload
-------------------

Overloading within the possible to keep ABI-compatibility.

Syntax::

	#pragma for TARGET-LANG overload SPECIFIER-QUALIFIER-LIST DECLARATOR

Example::

	#pragma for Ada overload void printf(char const *, char const *)
	#pragma for Ada overload void printf(char const *, int, int)

pragma for-type
---------------

Using the types in interface library instead of built-in types.

Syntax::

	#pragma for TARGET-LANG type TYPE-NAME = "TARGET-LANG-TYPE"

Example::

	#pragma for Ada type int = "Interfaces.C.int"
	#pragma for Ada type char * = "Interfaces.C.Strings.chars_ptr"

pragma instance
---------------

Giving the macro a type.

Syntax::

	#pragma instance SPECIFIER-QUALIFIER-LIST DECLARATOR'

(It's able to use string instead of identifier in DECLARATOR' for suppressing
macro expansion.)

Example::

	struct t {int x; int y};
	#define T_INIT {0, 0}
	#pragma instance struct t "T_INIT"
	
	#define max(a, b) (((a) > (b)) ? (a) : (b))
	#pragma instance int "max"(int, int)
	#pragma instance float "max"(float, float)
