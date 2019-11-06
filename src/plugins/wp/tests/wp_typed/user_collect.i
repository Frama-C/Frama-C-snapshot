/* run.config
   STDOPT: +"-wp-no-extensional"
*/
/* run.config_qualif
   STDOPT: +"-wp-no-extensional"
*/

int k ;
int p[10] ;

struct S { int f[2] ; } ;
struct S q[10] ;

/*@
  requires 0 <= k < 10 ;
  ensures K: k == \old(k) + 1 ;
  ensures P: p == { \old(p) \with [\old(k)] = x } ;
  assigns k,p[k] ;
*/
void job(int x)
{
  p[k++] = x ;
}

/*@
  requires 0 <= k < 10 ;
  ensures K: k == \old(k) + 1 ;
  ensures Q: q == { \old(q) \with [\old(k)] = s } ;
  assigns k,q[k] ;
*/
void job2(struct S s)
{
  q[k++] = s ;
}

/*@
  requires 0 <= k < 10 ;
  ensures K: k == \old(k) + 1 ;
  ensures Q: q == { \old(q) \with [\old(k)] = s } ;
  assigns k,q[k] ;
*/
void job3(struct S s)
{
  q[k].f[0] = s.f[0] ;
  q[k].f[1] = s.f[1] ;
  k++;
}

/*@
  requires 0 <= k < 9 ;
  ensures K: k == \old(k)+2 ;
  ensures P1: p[\old(k)+0] == x1 ;
  ensures P2: p[\old(k)+1] == x2 ;
*/
void caller(int x1 , int x2)
{
  job(x1);
  job(x2);
}

/*@
  requires 0 <= k < 9 ;
  ensures K: k == \old(k)+2 ;
  ensures Q1: q[\old(k)+0] == s1 ;
  ensures Q2: q[\old(k)+1] == s2 ;
  ensures R: q == {{ \old(q) \with [\old(k)] = s1 } \with [\old(k)+1] = s2 } ;
*/
void caller2(struct S s1 , struct S s2)
{
  job2(s1);
  job2(s2);
}

/*@
  requires 0 <= k < 9 ;
  ensures K: k == \old(k)+2 ;
  ensures Q1: q[\old(k)+0] == s1 ;
  ensures Q2: q[\old(k)+1] == s2 ;
  ensures R: q == {{ \old(q) \with [\old(k)] = s1 } \with [\old(k)+1] = s2 } ;
*/
void caller3(struct S s1 , struct S s2)
{
  job3(s1);
  job3(s2);
}




