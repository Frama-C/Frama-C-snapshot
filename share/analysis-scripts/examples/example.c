#include <string.h>

char s[10], t[10];

int f()
{
  memset(s, 0, 10);
  memcpy(t, s, 10);
  return 42;
}

void main(void)
{
  f();
}


#ifdef __FRAMAC__

int my_main(void)
{
  return f();
}

#endif
