/* run.config
   OPT: -safe-arrays
   OPT: -unsafe-arrays
 */

struct { int f[10]; } s,*p;
int a[10];

/*@ 
  requires \valid(p);
  ensures ARRAYS: \valid(&a[..]);
  ensures STRUCT: \valid(&s.f[..]);
  ensures INDIRP: \valid(&p->f[..]);
*/
void f(void) { }
