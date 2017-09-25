static inline void func1(void);
static inline void func1(void) {}

inline void func2(void);
inline void func2(void) {}

extern inline __attribute__((__gnu_inline__)) void func3(void);
extern inline __attribute__((__gnu_inline__)) void func3(void) {}
