/*  run.config
 STDOPT: +"-sparecode-debug 0 -no-warn-signed-overflow -sparecode"
 STDOPT: +"-sparecode-debug 0 -warn-signed-overflow -sparecode"
*/

/* The purpose of these tests is to check if the conditions are removed
 * when the branch is statically known. */

int f (int a) {
  int c = a+1;
  return (c > 0) ? 1 : 0;
}

int main (int x) {
  //@ assert x>5;
  if (x > 5) {
    int y = f(x);
    if (y < 2) // always true
      return f(x);
    else return -1;
  } else {
    return 4;
  }
}

