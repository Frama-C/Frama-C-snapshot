/* run.config
   STDOPT: #"-no-val-builtins-auto -slevel 1000 -val-builtin memcpy:Frama_C_memcpy"
*/ // slevel is used to unroll loops

#include "string.c"

void test_memcpy()
{
  char dest[6], src[6] = "hello";
  char *p = memcpy(dest, src, 6);
  //@assert p == dest;
  //@assert dest[0] == 'h';
  //@assert dest[4] == 'o';
  //@assert dest[5] == '\0';
  char src2[5] = "a\0b\0";
  memcpy(dest, src2, 5);
  //@assert dest[1] == '\0';
  //@assert dest[2] == 'b';
  memcpy(dest, NULL, 0);
  p = memcpy(NULL, NULL, 0);
  //@assert p == \null;
  int x = 0x12093841;
  memcpy(dest, NULL, 0);
  p = memcpy(NULL, NULL, 0);
  //@assert p == \null;
  memcpy(dest, &x, 4);
  //@assert dest[0] == 0x41;
  //@assert dest[3] == 0x12;
}

void test_memmove()
{
  char buf[6] = {1, 2, 3, 4, 5, 6};
  char *s, *d;
  s = buf;
  d = buf + 2;
  Frama_C_show_each_s0(s[0]);
  Frama_C_show_each_s0(s[1]);
  Frama_C_show_each_s0(s[2]);
  Frama_C_show_each_s0(s[3]);
  Frama_C_show_each_s0(d[0]);
  Frama_C_show_each_s0(d[1]);
  Frama_C_show_each_s0(d[2]);
  Frama_C_show_each_s0(d[3]);
  char *p = memmove(d, s, 4);
  //@assert p == d;
  /*@assert buf[0] == 1 && buf[1] == 2 && buf[2] == 1 &&
            buf[3] == 2 && buf[4] == 3 && buf[5] == 4; */
  buf[2] = 3; buf[3] = 4; buf[4] = 5; buf[5] = 6;
  d = buf;
  s = buf + 2;
  memmove(d, s, 4);
  /*@assert buf[0] == 3 && buf[1] == 4 && buf[2] == 5 &&
            buf[3] == 6 && buf[4] == 5 && buf[5] == 6; */
  memmove(buf, buf, 4);
  //@assert buf[0] == 3 && buf[1] == 4 && buf[2] == 5 && buf[3] == 6;
}

void test_strlen()
{
  char *s = "hello";
  int n = strlen(s);
  //@assert n == 5;
  n = strlen("ab\0c");
  //@assert n == 2;
  n = strlen("");
  //@assert n == 0;
}

void test_memset()
{
  char dest[4] = {0, 0, 0, 0};
  char *p = memset(dest, 42, 3);
  //@assert p == dest;
  //@assert dest[2] == 42;
  //@assert dest[3] == 0;
  memset(dest, 9, 0);
  //@assert dest[0] == 42;
}

void test_strcmp(void)
{
  char hello[6] = "hello";
  hello[1] = 'a';
  int res = strcmp("hallo", hello);
  //@ assert res == 0;
  int res2 = strcmp("hall", hello);
  //@ assert res2 < 0;
  int res3 = strcmp("halloo", hello);
  //@ assert res3 > 0;
  int res4 = strcmp("Hallo", hello);
  //@ assert res4 < 0;
  int res5 = strcmp("", hello);
  //@ assert res5 < 0;
  int res6 = strcmp("a\0b", "a");
  //@ assert res6 == 0;
  int res7 = strcmp("", "\0");
  //@ assert res7 == 0;
}

void test_strcat(void)
{
  char s[10];
  s[0] = 0;
  char *p = strcat(s, "hello");
  //@assert p == s;
  //@assert s[0] == 'h' && s[4] == 'o' && s[5] == 0;
  s[4] = 0;
  s[5] = 'h';
  s[6] = 'e';
  s[7] = 'l';
  s[8] = 0;
  strcat(s, s+5);
  //@assert s[3] == 'l' && s[4] == 'h' && s[6] == 'l' && s[7] == 0;
  strcat(s, "");
  strcat(s, "x");
  //@assert s[7] == 'x' && s[8] == 0;
}

void test_strcpy(void)
{
  char s[7];
  char *p = strcpy(s, "hello");
  //@assert p == s;
  //@assert s[0] == 'h' && s[4] == 'o';
  strcpy(s, "654321");
  //@assert s[2] == '4' && s[6] == 0;
  strcpy(s, "");
  //@assert s[0] == 0;
}

void test_strncpy(void)
{
  char s[7];
  char *p = strncpy(s, "hello", 7);
  //@assert p == s;
  //@assert s[5] == 0 && s[6] == 0;
  strncpy(s, "bye", 3);
  //@assert s[2] == 'e' && s[3] == 'l';
  strncpy(s, "die", 0);
  //@assert s[0] == 'b';
  strncpy(s, "ab\0c", 5);
  //@assert s[3] == 0;
}

void test_strncmp() {
  char hello[6] = "hello";
  int res1 = strncmp("hallo", hello, 5);
  //@ assert res1 < 0;
  int res2 = strncmp("hallo", hello, 1);
  //@ assert res2 == 0;
  hello[1] = 'a';
  int res3 = strncmp("hallo", hello, 5);
  //@ assert res3 == 0;
  int res4 = strncmp("halloo", hello, 6);
  //@ assert res4 > 0;
  int res5 = strncmp("halloo", hello, 5);
  //@ assert res5 == 0;
  int res6 = strncmp("", hello, 5);
  //@ assert res6 < 0;
  int res7 = strncmp("", hello, 0);
  //@ assert res7 == 0;
  int res8 = strncmp("a\0b", "a\0c", 3);
  //@ assert res8 == 0;
}

void test_memcmp() {
  char hello[6] = "hello";
  int res1 = memcmp("hallo", hello, 5);
  //@ assert res1 < 0;
  int res2 = memcmp("hallo", hello, 1);
  //@ assert res2 == 0;
  int res3 = memcmp("a\0b", "a\0c", 2);
  //@ assert res3 == 0;
  int res4 = memcmp("a\0b", "a\0c", 3);
  //@ assert res4 < 0;
}

void test_strchr()
{
  char *s = "hello";
  char *p = strchr(s, 'h');
  //@assert s == p;
  p = strchr(s, 'H');
  //@assert p == \null;
  p = strchr(s, 'l');
  //@assert p == s+2;
  p = strchr(s, 0);
  //@assert p == s+5;
}

void test_strrchr()
{
  char *s = "hello";
  char *p = strrchr(s, 'h');
  //@assert s == p;
  p = strrchr(s, 'H');
  //@assert p == \null;
  p = strrchr(s, 'l');
  //@assert p == s+3;
  p = strrchr(s, 0);
  //@assert p == s+5;
}

void test_memchr()
{
  char s[6] = {1, 2, 3, 1, 2, 4};
  char *p = memchr(s, 1, 6);
  //@assert s == p;
  p = memchr(s, 5, 6);
  //@assert p == \null;
  p = memchr(s, 2, 6);
  //@assert p == s+1;
  p = memchr(s, 1, 0);
  //@assert p == \null;
  p = memchr(s, 5, 0);
  //@assert p == \null;
  p = memchr(s, 1, 1);
  //@assert p == s;
}

void test_memrchr()
{
  char s[6] = {1, 2, 3, 1, 2, 4};
  char *p = memrchr(s, 1, 6);
  //@assert s+3 == p;
  p = memrchr(s, 5, 6);
  //@assert p == \null;
  p = memrchr(s, 2, 6);
  //@assert p == s+4;
  p = memrchr(s, 1, 0);
  //@assert p == \null;
  p = memrchr(s, 5, 0);
  //@assert p == \null;
  p = memrchr(s, 1, 1);
  //@assert p == s;
}

void test_strstr()
{
  char *s = "hello";
  char *p = strstr(s, "h");
  //@assert s == p;
  p = strstr(s, "");
  //@assert s == p;
  p = strstr(s, "H");
  //@assert p == \null;
  p = strstr(s, "ll");
  //@assert p == s+2;
  p = strstr(s, "elo");
  //@assert p == \null;
  p = strstr(s, "low");
  //@assert p == \null;
  p = strstr(s, "lo");
  //@assert p == s+3;
  p = strstr(s, "hell");
  //@assert p == s;
}

int main(int argc, char **argv)
{
  test_memcpy();
  test_memmove();
  test_strlen();
  test_memset();
  test_strcmp();
  test_strncmp();
  test_memcmp();
  test_strcat();
  // strncat is not tested (code from the man page)
  test_strcpy();
  test_strncpy();
  test_strchr();
  test_strrchr();
  test_memchr();
  test_memrchr();
  test_strstr();
  // strerror not tested
  // strdup not tested (uses malloc)
  // strndup not tested (uses malloc)
  return 0;
}
