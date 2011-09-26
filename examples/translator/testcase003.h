#include "testcase003-sub.h"

/* "struct t" was used in another header before body */
struct the_struct {
	int f;
	struct the_struct *self_ptr;
};
