extern void f(void);
extern void another_f(void);

#define f another_f
/* use another_f as f */

extern void _g(void);
extern void another_g(void);

#define _g another_g
/* use another_g as _g */
