
//@ ensures *p == (x==0 ? 1 : 0) ;
void check(int x , int *p) { *p = (x==0) ; }
  
