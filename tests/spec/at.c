/* run.config
   DONTRUN: ghost label is discarded + label in annotations not functional yet
*/
int x;

//@ ensures x == 2+\old(x)+y;
int f(int y) {
  x += y;
 L1:
  x++;
  //@ ghost L2: ;
  x++;
  //@ assert \at(x,L1) == 1+\at(x,Pre)+y;
  //@ assert \at(x,L2) == 2+\at(x,Pre)+y;
  return x;
}

/*
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make at"
End:
*/
