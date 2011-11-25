void __attribute__((always_inline,__gnu_inline__)) func(
	int (__attribute__((__cdecl__)) *)(struct t *));

\

#define M(x) z##x
int *M(aaa);
#define aaa (M(aaa))
#define bbb aaa
