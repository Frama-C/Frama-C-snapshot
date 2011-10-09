/*@ ensures \result != c; */
int f (int c) {

  /*@ returns \result == 0; */
  if (c) return 0;
  return 42;

}

/*@ requires \valid(a);
    ensures *a > 0;
*/
int g(int *a) {
  *a++;
  /*@ 
    behavior neg:
    assumes *a < 0; 
    returns \old(*a) == -*a; 
  */
  if (*a < 0) { *a = -*a; return -1; }
  if (*a != 0) { *a++; return 0; }
  return 1;
}
