
/*@
  @ ensures \result > x;
  @ behavior x1:
  @   assumes x == 1;
  @   ensures \result == 3;
  @ behavior x2:
  @   assumes x == 2;
  @   ensures \result == 4;
  @
*/
int f (int x) {
  x++;
  //@ for x1: assert x == 2;
  //@ for x2: assert x == 3;
  return x+1;
}

/*@
    behavior bx :
       assumes x <= y;
       ensures \result == x;
    behavior by :
       assumes x > y;
       ensures \result == y;
    complete behaviors bx, by;
    disjoint behaviors bx, by;
*/
int min (int x, int y) {
  return (x < y) ? x : y;
}

/*@ requires n != 0;
  behavior pos :
    assumes n > 0;
    ensures \result == x/n;
  behavior neg :
    assumes n < 0;
    ensures \result == x/-n;
  complete behaviors pos, neg; // notice that this needs the requires hyp
*/
int bhv (int x, int n) {
  n = (n<0) ? -n : n;
  return x/n;
}

int stmt_contract (int c) {
  int x = 0;

  /*@ requires x == 0;
    @ ensures x > 0;
    */
  if (c)
    x = 3;
  else
    x = 5;
  return x;
}

int local_named_behavior (int x) {
  int y = 3;
  /*@ behavior xpos :
        assumes x > 0;
        ensures x > 3;
	*/
  x += y;
  return x;
}
int main (void) { return 0 ; }
