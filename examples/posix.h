#if defined(__FreeBSD__)
#include <sys/_types.h> /* __time_t for sys/timespec.h */
#include <sys/timespec.h> /* struct timespec */
#include <sys/param.h>
#endif
#if defined(__APPLE__)
#include <stdint.h> /* uint32_t for sys/socket.h */
#include <sys/types.h> /* user_addr_t for malloc/malloc.h */
#include <malloc/malloc.h>
#endif
#if defined (__linux__)
#include <limits.h> /* LONG_MAX for bits/posix1_lim.h */
#endif
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/socket.h>
#include <netdb.h>
#include <pthread.h>
#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"
