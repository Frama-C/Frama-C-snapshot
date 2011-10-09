int labels (void) {
 int x = 0 ;
 L1: L2:
  //@ assert \at(x,L1) == \at(x,L2) ;
  return x ;
}
