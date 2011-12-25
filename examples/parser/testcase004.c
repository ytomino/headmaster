#define F(X) X*X
int z = F(F(2)); /* 16 */

#define M(X) PREFIX_##X
#define N M(THAT)
int N;
#define THAT THIS

int g(){ return N; } /* return PREFIX_THAT; */

#define M2(X) M(X)

int h(){ return M2(THAT); } /* return PREFIX_THAT; */
