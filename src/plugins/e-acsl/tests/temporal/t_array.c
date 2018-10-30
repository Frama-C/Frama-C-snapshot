/* run.config
   COMMENT: Check temporal timestamps of arrays
*/

int main() {
  int a = 111,
      b = 222;

  int *src[3];
  src[0] = &a;
  src[1] = &b;

  /*@assert \valid(src[0]);   */
  /*@assert \valid(src[1]);   */
  /*@assert ! \valid(src[2]); */
  return 0;
}

