int T [10];
int (*p)[10];
void main (int c) {

/*@ assert \valid(T + c); // synthesized alarm caused by a memory access
*/
/*@ assert \valid(T);  */

  p = (int (*)[10])&T[5];

  if(!c) {
    /*@ assert \valid( *p);  // means that the first element of *p is valid ! */
  }

  T[c] = 4;

}
