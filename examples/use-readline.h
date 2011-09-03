#include <stdio.h> /* FILE */
#if defined(__APPLE__)
#include <sys/time.h> /* struct timeval */
#include <sys/termios.h> /* struct termios */
#endif
#include <readline/readline.h>

#pragma for Ada "_ctype.h" include "runetype.h"
/* with clause in package body is unimplemented, force to with. */
