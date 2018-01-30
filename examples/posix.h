#if defined(__APPLE__)
#define _DONT_USE_CTYPE_INLINE_
#include <stdint.h> /* uint32_t for sys/socket.h */
#include <strings.h> /* bcopy for FD_COPY */
#elif defined(__FreeBSD__)
#define _DONT_USE_CTYPE_INLINE_
#elif defined (__gnu_linux__)
#include <limits.h> /* LONG_MAX for bits/posix1_lim.h */
#endif
#include <sys/types.h> /* before other system headers */
#include <sys/ucontext.h> /* before signal.h */
#include <sys/select.h>
#include <sys/resource.h> /* before sys/time.h */
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/syscall.h>
#if defined(__APPLE__)
#include <sys/vm.h> /* before sys/sysctl.h */
#include <sys/attr.h> /* before unistd.h */
#endif
#include <sys/sysctl.h>
#include <sys/mman.h>
#include <unistd.h>
#if defined (__gnu_linux__)
#include <stdlib.h> /* before sys/wait.h */
#endif
#include <sys/wait.h>
#if defined(__gnu_linux__)
#include <sys/stat.h> /* before fcntl.h */
#include <fcntl.h>
#else
#include <fcntl.h> /* before sys/stat.h */
#include <sys/stat.h>
#endif
#include <sys/file.h>
#include <poll.h>
#if defined(__gnu_linux__)
#define _SYS_SOCKET_H
#include <bits/socket.h> /* before netinet/in.h */
#include <netinet/in.h> /* before sys/socket.h */
#undef _SYS_SOCKET_H
#endif
#include <sys/socket.h> /* before sys/mount.h */
#include <netdb.h>
#if !defined(__gnu_linux__)
#include <netinet/in.h> /* after netdb.h in FreeBSD */
#endif
#include <sys/mount.h>
#if defined(__gnu_linux__)
#include <sys/statfs.h>
#endif
#include <dirent.h>
#include <fnmatch.h>
#include <termios.h>
#if defined(__APPLE__) || defined(__FreeBSD__)
#define _SYS_SOCKIO_H_ /* sys/sockio.h has many bad macros */
#endif
#include <sys/ioctl.h>
#if defined(__APPLE__) || defined(__FreeBSD__)
#undef _SYS_SOCKIO_H_
#endif
#include <pthread.h>
#include <dlfcn.h>
#if !defined(__FreeBSD__) || __FreeBSD__ >= 8
#include <spawn.h>
#endif
#if !defined(__gnu_linux__)
#include <pwd.h>
#include <grp.h>
#endif
#if defined(__APPLE__)
#undef _DONT_USE_CTYPE_INLINE_
#include <malloc/malloc.h>
#include <copyfile.h>
#include <mach/mach_time.h>
#define _ARCHITECTURE_BYTE_ORDER_H_ /* headmaster can not translate some inline functions */
#include <mach-o/dyld.h>
#undef _ARCHITECTURE_BYTE_ORDER_H_
#include <crt_externs.h> /* after mach-o/dyld.h */
#elif defined(__FreeBSD__)
#undef _DONT_USE_CTYPE_INLINE_
#include <sys/param.h>
#include <malloc_np.h>
#include <pthread_np.h>
#include <link.h>
#elif defined(__gnu_linux__)
#include <sys/sendfile.h>
#include <sys/statvfs.h>
#include <link.h>
#undef __USE_GNU /* avoiding circular dependency between libio.h and stdio.h */
#undef __USE_XOPEN2K8 /* avoiding circular dependency between wchar.h and stdio.h */
#include <libio.h> /* before stdio.h */
#undef _SC_NPROCESSORS_ONLN
#include <malloc.h>
#include <pwd.h> /* after stdio.h in Linux */
#include <grp.h>
#endif

#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"

#if defined(__gnu_linux__)
#pragma for Ada "signal.h" monolithic_include "bits/sigaction.h"
#pragma for Ada "signal.h" monolithic_include "bits/signum.h"
#pragma for Ada "signal.h" monolithic_include "bits/signum-generic.h"
#endif
