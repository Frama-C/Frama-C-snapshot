/* run.config
   STDOPT:
*/
#include "share/builtin.h"

int main() {
  unsigned char t[6] = { 0xef, 0xcd , 0xab , 0x00, 0x01, 0x01 };
  char s[6] =  { 0xef, 0xcd , 0xab , 0x00, 0x01, 0x01 };
  const char* s1 = "hello world\n";
  const char* s2 = "bla+hello world\n";

  int x = 0x00abcdef;
  int y = 0x01abcdef;

  int z = Frama_C_memcmp(&x,&x,4);
  //@ assert(z == 0);

  int a = Frama_C_memcmp(&x,&y,4);
  //@ assert(a < 0);

  int b = Frama_C_memcmp(&y,&x,4);
  //@ assert(b > 0);

  int c = Frama_C_memcmp(&x,t,4);
  //@ assert(c == 0);

  int d = Frama_C_memcmp(t,&x,4);
  //@ assert(d == 0);

  int e = Frama_C_memcmp(s,&x,4);
  //@ assert(e == 0);

  int f = Frama_C_memcmp(&x,s,4);
  //@ assert(f == 0);

  //  int g = Frama_C_memcmp(&x,s,6);

  int h = Frama_C_memcmp(s1,s2+4,13);
  //@ assert(h == 0);

  return 0;
}
