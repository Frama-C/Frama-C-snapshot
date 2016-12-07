#include <string.h>

void test_strcmp(void)
{
  int res = strcmp("hello", "world");
  //@ assert res == 0;
}

void test_strcat(void)
{
  char string[10];
  string[0] = 0;
  strcat(string, "hello");
}



int main(int argc, char **argv)
{
  test_strcmp();
  test_strcat();
  return 0;
}
