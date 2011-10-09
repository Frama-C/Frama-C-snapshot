int t1[10], t2[10] ;

//@ logic int * a1 = t1 + 0;
//@ logic int * a2 = &t1[0];
//@ logic int * a3 = &*t1 ;
//@ logic int * a4 = t1 ; // should not be accepted
//@ logic int * a5 = (int *)t1;
//@ logic int * b1 = \let x = t1 + 0; x ;
//@ logic int * b2 = \let x = &t1[0]; x ;
//@ logic int * b3 = \let x = &*t1 ; x ;
//@ logic int * b4 = \let x = t1 ; x ; // should not be accepted

int main () {
  int i ;
  for (i=0 ; i < 10 ; i++) {
    t1[i] = 0 ;
    t2[i] = 0 ;
    }
   if (t1 == t2) {
     /* C tests the address of the first elements,
      * so the then-branch is dead. */
     //@ assert \false;
     }
   else {
      /* ACSL tests the contents of the arrays,
       * here they are the same. */
      //@ assert (t1==t2) ; // even with the previous C
     }
}
