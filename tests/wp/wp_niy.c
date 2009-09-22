/* run.config
OPT: -wp-mm 0  -journal-disable -wp-no-proof
*/
/* run.config_dev
OPT: -wp-mm 0  -journal-disable -wp-proof
*/
// No -debug in order to really see user messages.

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

/*@ 
 requires n >= 0;
 terminates \true;
 assigns \nothing;
 behavior b91 :
     assumes n <= 101;
     ensures \result == 91;
 behavior b100 :
     assumes n > 100;
     ensures \result == n - 10;
 decreases (101 - n);
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
