/* run.config
   DONTRUN: invalid C file
*/

typedef struct { int i1; int i2; } s;

/*@ requires
  @ \valid(x + i) && &x[i]->i1 != 0;
  @*/
int f (s x[], int i) {
  return 1 / (&x[i])->i1;
}
