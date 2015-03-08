/* a.h */
typedef struct s_t { int n; } t;
/*@ type invariant inv_t(t x) = x.n > 0; */

/* @ predicate p(int x) reads x; */

int i = 42;

/*@ predicate p{Here}(int x) = x >= i; */

/*@ axiomatic Bar { logic integer li; } */

/*@ ensures i == li; */
void test() { }

/*@ axiomatic Foo {
  type foo;
  logic foo ff(foo x,char * y);
  predicate fp(foo x, foo y);
  axiom fffp: \forall foo x, char* y; fp(x,ff(x,y)) && *y == 0;
  } */
