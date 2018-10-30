/* run.config*
   OPT: -eva @VALUECONFIG@ -warn-special-float none
*/

/* Tests on special float values NaN and infinites. */

volatile int rand;


/* All comparisons involving NaN are false, except for inequalities that are
   true. */
void nan_comparisons () {
  double n = 0.0 / 0.0;
  double d = rand ? -10. : 10.;
  int eq1 = (n == n) ? 1 : 0;
  int comp1 = (n < n) ? 1 : 0;
  int ne1 = (n != n) ? 1 : 0;
  int eq2 = (n == d) ? 1 : 0;
  int comp2 = (n < d) ? 1 : 0;
  int ne2 = (n != d) ? 1 : 0;
}

void main () {
  nan_comparisons ();
}
