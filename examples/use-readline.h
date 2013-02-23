#include <stdio.h> /* FILE */

#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#elif defined(__APPLE__)
#include <sys/time.h> /* struct timeval */
#include <sys/termios.h> /* struct termios */
#endif

#include <readline/readline.h>

#pragma for Ada "_ctype.h" include "runetype.h"
/* with clause in package body is unimplemented, force to with. */
