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

// should be rejected as set of sets
/*@
predicate test3(set<int> s1,set<int> s2) =
\subset(\union({\union (k + 1) | int k ; constraint: 0 <= k < 10},s2),s1);
@*/

/*@ ensures \subset(\result, \union(x,x+1,x-1));
    ensures \result \in \union(x,x+1,x-1); */
int h(int x, int c) { return c>0 ? x+1 : c<0 ? x-1: x; }

/*@ requires \valid((\union(a,b))[0..1]);*/
int foo(int **a, int **b) { return 0; }

/*@ predicate reject1{L}(char *a) = a[0..1] < 10; */
/*@ predicate reject2{L}(char *a) = a[0..1] > 10; */
/*@ predicate reject3{L}(char *a) = 10 < a[0..1]; */
/*@ predicate reject4{L}(char *a) = 10 > a[0..1]; */

/*@ predicate test_singleton_1(int* a, int x) = x == a[0..1]; */ 
/*@ predicate test_singleton_2(int *a) = a[0..1] == 1; */
/*@ predicate test_singleton_3(int *a) = a[0..1] == {1}; */
/*@ predicate test_set_of_elem(int *a) = a[0..1] == {1,2}; */

/*@ predicate reject_set_of_sets_1(int *a) = {a[0..1]} == {1}; */
/*@ predicate reject_set_of_sets_2(int *a) = a[0..1] == {\empty}; */
int A[100];
/*@ ensures \subset(\result, &A[0..]) ;
    ensures \result \in &A[0..] ; */
int *AA(void);

//@ logic set<integer> Sempty_1 = \empty ;
//@ logic set<integer> Sempty_2 = { } ;

//@ logic set<integer> Selems_1 = { 1, (int)2, (int)'3' } ;

//@ logic set<integer> Sadd_elem_1(set<integer> s, integer e) = \union(s,e) ;
//@ logic set<integer> Sadd_elem_2(set<integer> s, integer e) = \union(s,{e}) ;

//@ predicate rejected_Smember_and(set<integer> s, integer v1, integer v2) = v1 & v2 \in s ;
//@ predicate rejected_Smember_or (set<integer> s, integer v1, integer v2) = v1 | v2 \in s ;
//@ predicate rejected_Smember_and_or (set<integer> s, integer v1, integer v2) = v1 & v2 \in s && v1 | v2 \in s ;

//@ predicate Smember_and(set<integer> s, integer v1, integer v2) = (v1 & v2) \in s ;
//@ predicate Smember_or (set<integer> s, integer v1, integer v2) = (v1 | v2) \in s ;
//@ predicate Smember_and_or (set<integer> s, integer v1, integer v2) = (v1 & v2) \in s && (v1 | v2) \in s ;

//@ logic set<integer> Scomprehension(set<integer> s, integer mask ) = { (k | 1) | int k ; (k | mask) \in s };
