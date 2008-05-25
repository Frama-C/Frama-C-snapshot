/*@ predicate bound(int x, int y, int z) =
  @   x<=y<z && z>=y>x ;
  @*/

/*@ predicate bound2 (int x, int y, int z) =
  @   x <= y == z ;
  @*/

// not the same as above, see pr#16
/*@ predicate test(int x, int y, boolean z) =
  @   (x<=y) == z ;
  @*/

// sense of inequalities matters
/*@ predicate reject(int x, int y, int z, int t) =
  @   x <= y == z >= t;
  @*/
