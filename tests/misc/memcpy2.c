#include "share/libc/__fc_builtin.h"

int main(int c, char **v)
{
  char t[512];
  char s[512] = { 0 };
  int l;

  l = Frama_C_interval(0,511);
  Frama_C_memcpy(t, s, l);

  Frama_C_dump_each();

  l = Frama_C_interval(0,512);
  Frama_C_memcpy(t, s, l);

  l = Frama_C_interval(1,512);
  Frama_C_memcpy(t, s, l);
}
