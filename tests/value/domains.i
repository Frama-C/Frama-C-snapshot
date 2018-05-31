/* run.config*
   STDOPT: #"-eva-sign-domain -eva-equality-domain -eva-bitwise-domain -eva-symbolic-locations-domain -eva-gauges-domain -slevel 2"
*/

/*  Tests five domains together. */
void main (int a) {
  int b, i, k, r;
  /* Tests the equality domain: b is reduced after the condition, no overflow. */
  b = a;
  if (a < 10)
    r = b + 1;
  /* Tests the symbolic locations domain: t[i] is smaller than 10, no overflow. */
  int t[2] = {a, a};
  i = a > 0;
  if (t[i] < 10)
    r = t[i] + 1;
  /* Tests the gauges domain: k==i during the loop, no overflow. */
  k = 0;
  while (k < 12) {
    k++;
    i++;
  }
  /* Tests the sign domain: no division by zero. */
  if (a != 0)
    r = 100 / a;
  /* Tests the bitwise domain: a == 8, no division by zero. */
  a = (a | 8) & 8;
  r = 10 / a;
}
