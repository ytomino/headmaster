typedef int a;
typedef a *pa;
typedef a aa[1];

typedef a b;
typedef b *pb;
typedef b ab[1];
typedef ab *pab;
typedef b const *pca;

/* new type */
enum e { e };
typedef enum e *pe;
typedef enum e ae[1];
typedef ae *pae;
typedef enum e const *pce;

/* new recursive type */
struct s;
typedef struct s *ps;
struct s { ps m; };
typedef struct s as[1];
typedef as *pas;
typedef struct s const *pcs;

/* alias of recursive type */
typedef struct s t;
typedef t *pt;
