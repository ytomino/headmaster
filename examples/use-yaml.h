#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#elif defined(__APPLE__)
#include <sys/types.h> /* avoiding circular dependency */
#endif

#include <yaml.h>
