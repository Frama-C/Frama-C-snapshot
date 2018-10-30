/* run.config
   COMMENT: upgrading longlong to GMP
   STDOPT: +"-eva-ignore-recursive-calls"
*/

unsigned long long my_pow(unsigned int x, unsigned int n) {
  int tmp;
  if (n <= 1) return 1;
  tmp = my_pow(x, n / 2);
  tmp *= tmp;
  if (n % 2 == 0) return tmp;
  return x * tmp;
}

int main(void) {
  unsigned long long x = my_pow(2, 63);
  /*@ assert (2 * x + 1) % 2 == 1; */
  return 0;
}
