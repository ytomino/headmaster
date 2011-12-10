#define X "12345"
#define Y sizeof(X)

static inline int f(char a[]){
	char b[] = X;
	int c = sizeof(b);
	return (int)*a + (int)b[c - 1];
}
