/* run.config
OPT: -wp-mm 2 -wp-fct f -wp-debug 1 -journal-disable -wp-no-proof
OPT: -wp-mm 2 -wp-fct g_loc -wp-debug 1 -journal-disable -wp-no-proof
OPT: -wp-mm 2 -wp-fct g_param -wp-debug 1 -journal-disable -wp-no-proof
OPT: -wp-mm 2 -wp-fct g_glob -wp-debug 1 -journal-disable -wp-no-proof
*/
/* run.config_dev
OPT: -wp-mm 2 -wp-fct f -wp-debug 1 -journal-disable -wp-proof
OPT: -wp-mm 2 -wp-fct g_loc -wp-debug 1 -journal-disable -wp-proof
OPT: -wp-mm 2 -wp-fct g_param -wp-debug 1 -journal-disable -wp-proof
OPT: -wp-mm 2 -wp-fct g_glob -wp-debug 1 -journal-disable -wp-proof
*/

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
