typedef struct file_t FILE;
extern FILE *stderr;
int fprintf(FILE *, char *, ...);

#define eprintf(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
