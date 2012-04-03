#include <stddef.h>

unsigned long func1(unsigned long);

static inline size_t func2(size_t x)
{
	return func1(x);
}
