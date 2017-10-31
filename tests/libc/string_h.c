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

volatile int nondet;
void test_strstr(void)
{
  char *s = nondet ? "aba" : "bab";
  char *needle = nondet ? "a" : "b";
  char *res = strstr(s, needle);
  //@ assert res != 0;
}

void test_strncat(void)
{
  char data[100];
  data[0] = '\0';
  char source[100];
  //@ slevel 99;
  for (int i = 0; i < 99; i++) source[i] = 'Z';
  source[99] = '\0';
  strncat(data, source, 100);
}

struct s {
  char s1[30];
  char s2[30];
};

// this test crashes GCC (tested with v7.1.1) due to the non-respect of
// non-aliasing in strcpy
void crashes_gcc() {
  struct s s;
  char *ss = "ABCDEFGHIJKLMNOPQRSTUVWXYZ012";
  //@ slevel 30;
  for (int i = 0; i < 30; i++) s.s1[i] = ss[i];
  char *dest = s.s1+29;
  char *src = s.s1;
  strcpy(dest, src); // must produce at least a warning
}

int main(int argc, char **argv)
{
  test_strcmp();
  test_strcat();
  test_strstr();
  test_strncat();
  if (!nondet) crashes_gcc();
  return 0;
}
