/* run.config
   STDOPT: #"-no-val-builtins-auto -cpp-extra-args=-includeshare/libc/string.c -slevel-function strcpy:20,strncpy:5,strcmp:6,strchr:20,strrchr:20,strncat:4,memset:32,strlen:20,memcmp:8"
*/
/* This file has been adapted from libc-test, which is licensed under the
   following standard MIT license:

Copyright © 2005-2013 libc-test AUTHORS (Rich Felker, Szabolcs Nagy,
Kirill Ternovsky, John Spencer, Jens Gustedt, Alexander Monakov)

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#define T_LOC2(l) __FILE__ ":" #l
#define T_LOC1(l) T_LOC2(l)
#define t_error(...) t_printf(T_LOC1(__LINE__) ": " __VA_ARGS__)
int t_printf(const char *s, ...);

/* r = place to store result
 * f = function call to test (or any expression)
 * x = expected result
 * m = message to print on failure (with formats for r & x)
**/

#define TEST(r, f, x, m) (                                              \
                          ((r) = (f)) == (x) ||                         \
                          (t_error("%s failed (" m ")\n", #f, r, x), 0) )

#define TEST_S(s, x, m) (                                               \
                         !strcmp((s),(x)) ||                            \
                         (t_error("[%s] != [%s] (%s)\n", s, x, m), 0) )

int main(void)
{
  char b[32];
  char *s;
  int i;

  b[16]='a'; b[17]='b'; b[18]='c'; b[19]=0;
  TEST(s, strcpy(b, b+16), b, "wrong return %p != %p");
  TEST_S(s, "abc", "strcpy gave incorrect string");
  TEST(s, strcpy(b+1, b+16), b+1, "wrong return %p != %p");
  TEST_S(s, "abc", "strcpy gave incorrect string");
  TEST(s, strcpy(b+2, b+16), b+2, "wrong return %p != %p");
  TEST_S(s, "abc", "strcpy gave incorrect string");
  TEST(s, strcpy(b+3, b+16), b+3, "wrong return %p != %p");
  TEST_S(s, "abc", "strcpy gave incorrect string");

  TEST(s, strcpy(b+1, b+17), b+1, "wrong return %p != %p");
  TEST_S(s, "bc", "strcpy gave incorrect string");
  TEST(s, strcpy(b+2, b+18), b+2, "wrong return %p != %p");
  TEST_S(s, "c", "strcpy gave incorrect string");
  TEST(s, strcpy(b+3, b+19), b+3, "wrong return %p != %p");
  TEST_S(s, "", "strcpy gave incorrect string");

  TEST(s, memset(b, 'x', sizeof b), b, "wrong return %p != %p");
  TEST(s, strncpy(b, "abc", sizeof b - 1), b, "wrong return %p != %p");
  TEST(i, memcmp(b, "abc\0\0\0\0", 8), 0, "strncpy fails to zero-pad dest");
  TEST(i, b[sizeof b - 1], 'x', "strncpy overruns buffer when n > strlen(src)");

  b[3] = 'x'; b[4] = 0;
  strncpy(b, "abc", 3);
  TEST(i, b[2], 'c', "strncpy fails to copy last byte: %hhu != %hhu");
  TEST(i, b[3], 'x', "strncpy overruns buffer to null-terminate: %hhu != %hhu");

  TEST(i, !strncmp("abcd", "abce", 3), 1, "strncmp compares past n");
  TEST(i, !!strncmp("abc", "abd", 3), 1, "strncmp fails to compare n-1st byte");

  strcpy(b, "abc");
  TEST(s, strncat(b, "123456", 3), b, "%p != %p");
  TEST(i, b[6], 0, "strncat failed to null-terminate (%d)");
  TEST_S(s, "abc123", "strncat gave incorrect string");

  strcpy(b, "aaababccdd0001122223");
  TEST(s, strchr(b, 'b'), b+3, "%p != %p");
  TEST(s, strchr(b, 'e'), 0, "%p != %p");
  TEST(s, strrchr(b, 'b'), b+5, "%p != %p");
  TEST(s, strrchr(b, 'e'), 0, "%p != %p");

  return 0;
}
