/*@ ensures *x == 3;
    assigns *x;
 */
void f (int * x) {
  *x = 3;
}

//@ ensures \result == 3;
int g_loc (void) {
  int x;
  f (&x);
  return x;
}
//@ ensures \result == 3;
int g_param (int x) {
  f (&x);
  return x;
}

// don't define 'x' as a global variable because CIL then renames other 'x'
// and it hides the problem.
int y;

//@ ensures *y == 3;
void fy (int * y) {
  *y = 3;
}

//@ ensures y == 3;
void g_glob (void) {
  f (&y);
}
int main (void) {return 0;}
