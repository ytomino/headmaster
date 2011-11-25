#if defined(__FreeBSD__)
#include <sys/_types.h> /* __time_t */
#include <sys/timespec.h> /* struct timespec */
#include <sys/param.h>
#endif
#if defined(__APPLE__)
#include <stdint.h> /* uint32_t for sys/socket.h */
//#include <malloc/malloc.h>
#endif
#include <sys/fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/socket.h>
#include <pthread.h>
#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"
