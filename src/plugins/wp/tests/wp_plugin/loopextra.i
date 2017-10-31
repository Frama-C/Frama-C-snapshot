
void f (int n) {
  for (int i = 0; i < n; i++) {
    /*@ assert \at(i,LoopEntry) == 0; */
    int j = 0;
    while (j++ < i) {
      /*@ assert \at(j,LoopEntry) == 0; */
      /*@ assert \at(j,LoopCurrent) + 1 == j; */
    }
  }
}
