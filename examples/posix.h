#if defined(__APPLE__)
#include <stdint.h> /* uint32_t for sys/socket.h */
#endif
#if defined (__linux__)
#include <limits.h> /* LONG_MAX for bits/posix1_lim.h */
#endif
#include <sys/types.h> /* before other system headers */
#include <sys/time.h>
#include <sys/syscall.h>
#if defined(__APPLE__)
#include <sys/vm.h> /* before sys/vm.h */
#endif
#include <sys/sysctl.h>
#include <sys/ucontext.h>
#include <sys/mman.h>
#include <unistd.h>
#if defined (__linux__)
#include <stdlib.h> /* before sys/wait.h */
#endif
#include <sys/resource.h>
#include <sys/wait.h>
#include <pwd.h>
#include <grp.h>
#if defined(__linux__)
#include <sys/stat.h> /* before fcntl.h */
#include <fcntl.h>
#else
#include <fcntl.h> /* before sys/stat.h */
#include <sys/stat.h>
#endif
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/mount.h>
#include <dirent.h>
#include <fnmatch.h>
#include <termios.h>
#include <netdb.h>
#include <netinet/in.h>
#include <pthread.h>
#include <dlfcn.h>
#if defined(__APPLE__)
#include <crt_externs.h>
#include <malloc/malloc.h>
#include <spawn.h>
#include <copyfile.h>
#elif defined(__FreeBSD__)
#include <sys/param.h>
#include <malloc_np.h>
#include <pthread_np.h>
#include <link.h>
#elif defined(__linux__)
#include <sys/statvfs.h>
#include <spawn.h>
#include <malloc.h>
#include <link.h>
#endif

#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"

#if defined(__linux__)
#pragma for Ada "signal.h" monolithic_include "bits/sigaction.h"
#pragma for Ada "signal.h" monolithic_include "bits/signum.h"
#endif
