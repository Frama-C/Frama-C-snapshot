int *p,*q,*r;
/*@ //idem allocates \nothing
  @ behavior a:
      requires *p==0 ;
      //idem allocates \everything;
 */
void f1 (void) { return ; }

/*@ requires !q ;
    //idem allocates \nothing
  @ behavior a:
      requires p;
      frees p ;
 */
void f2 (void) { return ; }

/*@ requires i<0 ;
    //idem allocates \nothing
  @ behavior a:
      requires p;
      frees r ;
      allocates q, \old(r);
      frees p ;
 */
void f3 (int i) { 
  /*@ //idem loop allocates \nothing
    @ for a: loop invariant i <0;
             //idem loop allocates \everything;
   */
  while (i) {
    i--;
    p++;
  }
}

