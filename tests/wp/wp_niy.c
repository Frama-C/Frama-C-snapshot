/* run.config
   DONTRUN: don't run a test which raises an exception
*/
/*@ 
 requires n >= 0;
 terminates \true;
 decreases 101 - n;
 assigns \nothing;
 behavior b91 :
     assumes n <= 101;
     ensures \result == 91;
 behavior b100 :
     assumes n > 100;
     ensures \result == n - 10;
*/
int f91 (int n) {
  if ( n > 100 )
    return n - 10;
  else
    return f91(f91(n+11));
}

// This is not handled yet, 
// but I am not sure that we should accept to take the address of \result...
//@ ensures \valid (&(\result));
int addr_result (void) {
  return 0;
}

int loop2 (int n) {
  int i, s = 0;
  for (i = 0; i < n; i++) {
    //@ invariant 0 <= i < n ;
    s++;
  }
  return s;
}

/*@ requires c > 0;
 */
int goto_loop (int c) {
  int x = 1;
  L : x++;
      //@ invariant (0 < c <= \at(c, Pre)) && x == 2 + (\at(c, Pre) - c);
  if (--c > 0) goto L;
  return x;
}

/* Doc example 2.45 */
int abrupt (int x) {
    while (x > 0) {
      /*@ breaks x % 11 == 0 && x == \old (x );
        @ continues (x +1) % 11 != 0 && x % 7 == 0 && x == \old (x ) -1;
        // @ returns ( \result +2) % 11 != 0 && ( \result +1) % 7 != 0
        // @           && \result % 5 == 0 && \result == \old (x ) -2;
        @ ensures (x +3) % 11 != 0 && ( x +2) % 7 != 0 && (x +1) % 5 != 0
        @           && x == \old (x ) -3;
        @ */
      {
         if   (x % 11 == 0)      break ;
         x--;
         if   (x % 7 == 0)    continue ;
         x--;
         if   (x % 5 == 0)    return x;
         x--;
      }
    }
    return    x;
}

int main (void) { return 0 ; }
