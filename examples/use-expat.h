#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#elif defined(__APPLE__)
#include <sys/resource.h> /* avoiding circular dependency */
#include <sys/signal.h> /* avoiding circular dependency */
#endif

#include <expat.h>
