/* preprocessor */

\

#define M(x) z##x
int *M(aaa);
#define aaa (M(aaa))
#define bbb aaa

int *r(){ return bbb; } /* return zaaa; */

#define F(X) X*X
int z = F(F(2)); /* 16 */

#define M(X) PREFIX_##X
#define N M(THAT)
int N; /* PREFIX_THAT */
#define THAT THIS

int g(){ return N; } /* return PREFIX_THAT; */

#define M2(X) M(X)

int PREFIX_THIS;
int h(){ return M2(THAT); } /* return PREFIX_THIS; */

#define X100(a) a##00

enum {
	the_256 = X100(0x1),
	the_100 = X100(1),
	the_64 = X100(01)
};

#define Y100(a) X100(a)
#define H1 0x1

enum {
	another_256 = Y100(H1)
};

#define ALIAS valid_name
#define ALIAS2 ALI ## AS

int ALIAS;
int f_valid_name(){ return ALIAS2; } /* return valid_name; */

#define __MSABI_LONG(x)  x ## l
#define OLEIVERB_SHOW (__MSABI_LONG(-1))

long z = OLEIVERB_SHOW;
