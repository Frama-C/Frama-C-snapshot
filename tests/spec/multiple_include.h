/* a.h */
typedef struct s_t { int n; } t;
/*@ type invariant inv_t(t x) = x.n > 0; */

/* @ predicate p(int x) reads x; */

/* if uncommented, should lead to an error */
static int i = 42;

/*@ predicate p{Here}(int x) = x >= i; */
