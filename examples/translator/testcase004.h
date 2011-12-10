struct s1 {
	int m;
	int n;
};

struct s2 {
	int m : 8;
	int n;
};

struct s3 {
	int m;
	int n : 8;
};

struct s4 {
	int m : 8;
	int n : 8;
};

struct s5 {
	int m;
	int : 4 * (16 - sizeof(struct s1));
	int : 4 * (16 - sizeof(struct s1));
};
