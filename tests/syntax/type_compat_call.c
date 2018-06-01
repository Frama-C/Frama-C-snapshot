/*run.config
  STDOPT: #"-machdep x86_32"
 */
#include <stdint.h>

void f(int32_t const *p);
void g(int32_t *const p);
void h(uint8_t const *p);
void m(int8_t const *p);

int main() {
  int const i = 42;
  f(&i); // compatible
  g(&i); // incompatible
  char const c = 'c';
  signed char const s = 's';
  unsigned char const u = 'u';
  h(&c); // incompatible
  h(&u); // compatible
  m(&c); // compatible
  m(&s); // incompatible
  return 0;
}
