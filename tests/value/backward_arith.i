/* run.config*
*/

/* Test the soundness of arithmetic backward propagators.  */

volatile int nondet;

void unsigned_neg () {
  unsigned int x = nondet;
  unsigned int minus_ten = -10; /* minus_ten = 4294967286. */
  if (-x == minus_ten)
    Frama_C_show_each_ten(x);
  else
    Frama_C_show_each_not_ten(x);
  if (-x < minus_ten)
    Frama_C_show_each_greater_than_ten_or_zero(x);
  else
    Frama_C_show_each_smaller_than_ten_but_zero(x);
  if (-x == 10)
    Frama_C_show_each_minus_ten(x); /* 4294967286 */
  else
    Frama_C_show_each_not_minus_ten(x);  /* not 4294967286 */
  if (-x < 10)
    Frama_C_show_each_greater_than_minus_ten_or_zero(x); /* > 4294967286 or 0 */
  else
    Frama_C_show_each_smaller_than_minus_ten_but_zero(x); /* <= 4294967286 but 0 */
}


int main () {
  unsigned_neg ();
  return 0;
}
