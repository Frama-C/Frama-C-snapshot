/* run.config
   GCC:
   OPT: -val -deps -out -input -journal-disable
   OPT: -val -deps -out -input -main main1 -journal-disable
   OPT: -val -deps -out -input -main main2 -journal-disable
*/
long i,j,x,k,l,m,n,d,a,b;
long *ptr;

//-----------------------------------------
void main(int c) {
  a = 333;
  ptr = c ? &a : &b ;
  *ptr = 77;
  i=*ptr+1-1;
  return;
// needs relations to be accurate
}
//-----------------------------------------
void main1(int c) {
  i = c?3:4;

  x = i;
  j = x - i;
}
//-----------------------------------------
// Just a test for dependencies
void f2 (int arg) {
  b = arg + l;
  a = arg + m ;
}
void g2 (int arg) {
  a = arg + n ;
}
void (*tab_ptr_fct2[2])(int) = { &f2, &g2};
void main2(int c,int arg) {
  j = c?0:1;
  (*tab_ptr_fct2[j])(arg); // Dependency of j are taken into account.
}
//-----------------------------------------
