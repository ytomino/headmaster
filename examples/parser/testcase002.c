/* rare syntax */

struct t {int e;} u = {.e = 10};
int v[1] = {[0] = 20};
short w[] = L"wi" L"de";

int f(x, y)
	int x;
	int y;
{
	int *p = (int[4]){1, 2, 3, 4};
	for(int i = 0; i < 3; ++i);
	switch(x){
	case 1:;
	default:;
	}
	return x + y;
}
