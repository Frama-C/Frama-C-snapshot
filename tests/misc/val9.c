/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -main f -journal-disable
   OPT: -memory-footprint 1 -val -deps -out -input -main g -journal-disable
*/
int T[10]={1,2,3};
int i,a,b;

void f() {
  for (i = 0; i <= 8; i++) {
    T[i] = i;
    *((int*)((char*)&(T[i]) + 1)) = 0;
    }

}


void g() {
  a = 1;
  if (b) i=5; else i=6;
  a=3;
  if (i>=2) { a = i ; T[i] = 7 ; }

  for (i = 0; i <= 8; i++) {
    *(char *) &a = 1;
b = a;

   *((int*)(((char*)&(T[i])) + 1)) = 0;
    }

}
