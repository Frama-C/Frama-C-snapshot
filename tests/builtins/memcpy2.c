#include "__fc_builtin.h"
#include "string.h"
int main(int c, char **v)
{
  char t[512];
  char s[512] = { 0 };
  int l;

  l = Frama_C_interval(0,511);
  memcpy(t, s, l);

  Frama_C_dump_each();

  l = Frama_C_interval(0,512);
  memcpy(t, s, l);

  l = Frama_C_interval(1,512);
  memcpy(t, s, l);
}
