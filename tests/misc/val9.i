/* run.config
   GCC:
   OPT: -val -deps -out -input -main f -journal-disable
*/
int TT[10]={1,2,3};
int T[10]={1,2,3};
int i,a,b;
int a7, b7;

int O1[20];
int O2[20];
int *p;

int x2,*b2,a2;

void f() {
  for (i = 0; i <= 8; i++) {
    TT[i] = i;
    *((int*)((char*)&(TT[i]) + 1)) = 0;
    }

  a = 1;
  if (b) i=5; else i=6;
  a=3;
  if (i>=2) { a = i ; T[i] = 7 ; }

  for (i = 0; i <= 8; i++) {
    *(char *) &a = 1;
b = a;

   *((int*)(((char*)&(T[i])) + 1)) = 0;
    }




  a7 = 'a';
  *(char *) &a7 = 1;
  b7 = (char)a7;


  ((int*)O1)[1]=17;
  ((char*)O1)[1]=18;


  ((int*)O2)[0]=10;
  ((char*)O2)[1]=11;

  O1[6]=0;
  p=O1+9;
  *p=1;


  x2 = 777;
  a2 = (int)&x2;
  b2 = (int*) a2;
  *((int*)a2) = 0;
  *b2=*b2+1;
}
