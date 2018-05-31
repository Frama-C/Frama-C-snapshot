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
  char string2[10];
  string2[0] = 0;
  strcat(string2, string);
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

void test_strtok() {
  if (nondet) {
    strtok(NULL, " "); // must fail
    //@ assert unreachable: \false;
  }
  char buf[2] = {0};
  char *a = strtok(buf, " ");
  //@ assert a == \null || \subset(a, buf+(0..));
  char *b = strtok(NULL, " ");
  //@ assert b == \null || \subset(b, buf+(0..));
  char buf2[4] = "abc";
  char *p = strtok(buf2, "b");
  //@ assert p == \null || \subset(p, buf2+(0..));
  char *q = strtok(NULL, "c");
  //@ assert q == \null || \subset(p, buf2+(0..));
}

void test_strtok_r() {
  if (nondet) {
    strtok_r(NULL, " ", NULL); // must fail
    //@ assert unreachable: \false;
  }
  char *saveptr;
  char buf[2] = {0};
  char *a = strtok_r(buf, " ", &saveptr);
  if (nondet) {
    strtok_r(buf, " ", NULL); // must fail
    //@ assert unreachable: \false;
  }
  //@ assert a == \null || \subset(a, buf+(0..));
  char *b = strtok_r(NULL, " ", &saveptr);
  Frama_C_show_each_saveptr(saveptr);
  //@ assert b == \null || \subset(b, buf+(0..));
  char buf2[4] = "abc";
  char *p = strtok_r(buf2, "b", &saveptr);
  //@ assert p == \null || \subset(p, buf2+(0..));
  char *q = strtok_r(NULL, "c", &saveptr);
  //@ assert q == \null || \subset(p, buf2+(0..));
}

int main(int argc, char **argv)
{
  test_strcmp();
  test_strcat();
  test_strstr();
  test_strncat();
  if (!nondet) crashes_gcc();
  test_strtok();
  test_strtok_r();
  return 0;
}
