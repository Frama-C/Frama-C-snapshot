/* run.config
   OPT: -rte -warn-signed-overflow -print -rte-no-all -rte-precond -journal-disable
*/

//@ assigns *p \from \union(*(char*)p,*q);
extern void f(int* p, int* q);

//@ assigns *p \from \union(*p, \union(*r,*q));
extern void ff(int* p, int* q, int* r);

//@ assigns *p \from \inter(*(char*)p,*q);
extern void h(int* p, int* q);

//@ assigns \union(*p,*q);
extern void g(int* p, int* q);

/*@ assigns \at(*p,Post), \at(*p,Pre), *p ;
 */
extern void gg(int* p);

int X, Y ;
//@ assigns \union(X, Y) ;
void hh();

int main() {
  int x,y,z;
  f(&x,&y);
  ff(&x,&y,&z);
  g(&x,&y);
  h(&x,&y);
  gg(&x);
  hh();
}
