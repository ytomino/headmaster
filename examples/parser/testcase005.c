typedef int t;
typedef int other_t;

/* use typedef-ed name as new name */

void f1()
{
	int t;
}

void f2()
{
	struct { int m; } t;
}

void f3()
{
	other_t t;
}

void f4()
{
	typedef other_t t;
}

struct s1
{
	int t;
};

struct s2
{
	struct { int m; } t;
};

struct s3
{
	other_t t;
};

struct s4
{
	volatile other_t t;
};

/* anonymous struct */

union u1
{
	struct { int m1; };
	__extension__ struct { int m2; };
};
