/* run.config, run.config_2
   COMMENT: pointer substraction
*/

void f(const void *s, int c, unsigned long n) {
  const unsigned char *p = s;
  /*@ assert p - s == n - n; */
  /*@ assert p - s == 0; */
}

int main() {
  const char *s = "1234567890";
  f(s, '0', 11);
  return 0;
}
