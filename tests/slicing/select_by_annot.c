/* run.config
   EXECNOW: make -s tests/slicing/select_by_annot.opt
   CMD: tests/slicing/select_by_annot.opt
   OPT: -deps -lib-entry -main main
   CMD: bin/toplevel.opt
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma main
   OPT: -deps -lib-entry -main main -slice-print -slice-assert main
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma modifS -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f1 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f2 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f3 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f4 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f5 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f6 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f7 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-loop-inv f8 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f8 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-assert f8 -slicing-debug -no-slice-callers
   OPT: -deps -lib-entry -main main -slice-print -slice-pragma f9 -slicing-debug -no-slice-callers


*/
struct Tstr { int a; int b; } S;
int Sa ;

int f1(int cond) {
  int * p = &S.a ;
  if (cond) {
    //@ assert (cond != 0);
    Sa = *p ;
    }
  //@slice pragma expr *p;
  return Sa ;
}

int f2(int cond) {
  int * p = &S.a ;
  if (cond)
    //@ assert (cond != 0);
    Sa = *p ;
  //@slice pragma expr S.a;
  return Sa ;
}

int f3(int cond) {
  int * p = &S.a ;
  if (cond) {
    //@ slice pragma ctrl;
    Sa = *p ;
    }
  return Sa ;
}

int f4(int cond) {
  int * p = &S.a ;
  if (cond) {
    //@ slice pragma stmt;
    Sa = *p ;
    }
  return Sa ;
}

int f5(int cond) {
  int * p = &S.a ;
  if (cond) {
    //@ slice pragma expr 1;
    Sa = *p ;
    }
  return Sa ;
}

int f6(int cond) {
  int * p = &S.a ;
  //@ slice pragma stmt;
  if (cond) {
    Sa = *p ;
    Sa ++ ;
    }
  return Sa ;
}

int f7(int cond) {
  int * p = &S.a ;
  if (cond)
  //@ slice pragma stmt;
    {
      Sa = *p ;
      Sa ++ ;
    }
  return Sa ;
}


int f8(int cond) {
  int * p = &S.a ;
  //
  /*@ loop invariant cond >= 0 ;
    loop variant cond ; */
  while (cond)
    { //@ assert  cond <= \at(cond,Pre) ;
      //  assert S.a + cond == \at(S.a + cond,Pre) ;
      Sa = *p ;
      //@ slice pragma stmt;
      S.a ++ ;
      cond--;
    }
  return Sa ;
}

int X9, Y9, Z9 ;
void f9(int c1, int c2) {
  if (c1 > c2)
    goto L;
  c1 = c2 ;
  //@ slice pragma stmt;
  {L: X9 = c1 ;}
  Y9 = Z9 ;
  Z9 = c2 ;
}

void modifS (int a, int b) {
  S.a += a;
  S.b -= b;
  //@slice pragma expr S.a;
}
int new_int (void);
  int d;
int main (void) {
  int a = 0;
  int b = 0;
  int c = 0;
  if (d > 0) {
    //@ assert (b == 0);
    a = 1;
    }
  //@ slice pragma expr a+b;
  int x = a+b+c+d;
  modifS (a, b);
  // assert (d>0 => a == 1) && (!(d>0) => a==0);
  d = new_int ();
  f1(d) ;
  f2(d) ;
  f3(d) ;
  f4(d) ;
  f5(d) ;
  f6(d) ;
  f7(d) ;
  f8(d) ;
  f9(d,a) ;
  return x;
}
