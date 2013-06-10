struct foo { char bar[4]; };

/*@ assigns x->bar[0..3] \from x->bar[0..3]; */
int f(struct foo* x);

typedef char baz[4];

struct bli { baz bli; };

/*@ assigns x[0..3] \from y->bli[0..3]; */
int g(baz x,struct bli* y);

int main() {
  struct foo x;
  baz y;
  struct bli z;
  f(&x);
  g(y,&z);
  /*@ assert \separated(&x,&y[2]); */
  return 0;
}

/*@
predicate test1(set<int> s1,set<int> s2) =
\subset(s1,\union(s2,{k + 1 | int k ; constraint: 0 <= k < 10}));
@*/

/*@
predicate test2(set<int> s1,set<int> s2) =
\subset(\union({k + 1 | int k ; constraint: 0 <= k < 10},s2),s1);
@*/

/*@ ensures \subset(\result,\union(x,x+1,x-1)); */
int h(int x, int c) { return c>0 ? x+1 : c<0 ? x-1: x; }

/*@ requires \valid((\union(a,b))[0..1]);*/
int foo(int **a, int **b) { return 0; }
