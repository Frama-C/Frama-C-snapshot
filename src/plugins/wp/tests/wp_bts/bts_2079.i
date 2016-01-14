/* run.config_qualif
   OPT: -wp-prop=-Obs
 */

/*@ axiomatic S { predicate S(integer k); } */

typedef struct S { int f[40]; } ;

struct S a[3] ;
struct S *p;
int *q;
int *r;

/*@ 
  ensures Obs:P: S((p+k)-p);
  ensures Obs:Q: S((q+k)-q);
  ensures Obs:R: S((r+k)-q);
  ensures Eval:P: p+k-p == k ;
  ensures Eval:Q: q+k-q == k ;
*/
int main(int k) {
  p = a;
  q = &a[1].f[12];
  r = &a[2].f[17];
  return 0;
}
