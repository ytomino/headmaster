#include "testcase001-sub.h"
/* a type is declared in another header */

struct parent { struct child member[10]; };
/* use array of the type as member */

#define member_alias member
/* it have to be translated to
   function member_alias (Object : struct_parent) return struct_child_array10,
   The constrained array "struct_child_array10" should be declared *)
