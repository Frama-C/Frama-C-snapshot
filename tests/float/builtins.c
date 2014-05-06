#include <share/libc/__fc_builtin.h>

float f;
double d;

int main(int c, char **v)
{
  f = Frama_C_float_interval(-1.0, 1.0);
  d = Frama_C_double_interval(-1.0, 1.0);
}
