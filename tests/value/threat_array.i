int T [10];
int (*p)[5];
int (*q)[10];
void main (int c) {

/*@ assert \valid(T + c); // synthesized alarm caused by a memory access
*/
/*@ assert \valid(&T);  */
/*@ assert \valid(&T[0..9]);  */

  p = (int (*)[5])&T[5];
  q = (int (*)[10])&T[5];

  /*@ assert \valid(p);  */

  if(!c) {
    /*@ assert \valid(q);  */
  }

  T[c] = 4;

}
