#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#elif defined(__APPLE__)
#include <sys/types.h> /* avoiding circular dependency for <stdlib.h> */
#endif

#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>

#include <stdlib.h> /* free */
