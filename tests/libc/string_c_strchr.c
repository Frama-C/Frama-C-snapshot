/* run.config
   STDOPT: #"-cpp-extra-args=-includeshare/libc/string.c -slevel-function strchr:256,main:256 -val-builtin memcpy:Frama_C_memcpy -val-slevel-merge-after-loop main -no-val-builtins-auto"
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

static char buf[512];

#define N(s, c) {                                                       \
    char *q = strchr(s, c);                                             \
    if (q)                                                              \
      t_error("strchr(%s,%s) returned str+%d, wanted 0\n", #s, #c, q-s); \
  }

#define T(s, c, n) {                                                    \
    char *p = s;                                                        \
    char *q = strchr(p, c);                                             \
    if (q == 0)                                                         \
      t_error("strchr(%s,%s) returned 0, wanted str+%d\n", #s, #c, n);  \
    else if (q - p != n)                                                \
      t_error("strchr(%s,%s) returned str+%d, wanted str+%d\n", #s, #c, q-p, n); \
  }

int main(void)
{
  int i;
  char a[128];
  char s[256];

  for (i = 0; i < 128; i++)
    a[i] = (i+1) & 127;
  for (i = 0; i < 256; i++)
    *((unsigned char*)s+i) = i+1;

  N("\0aaa", 'a');
  N("a\0bb", 'b');
  N("ab\0c", 'c');
  N("abc\0d", 'd');
  N("abc abc\0x", 'x');
  N(a, 128);
  N(a, 255);

  T("", 0, 0);
  T("a", 'a', 0);
  T("a", 'a'+256, 0);
  T("a", 0, 1);
  T("abb", 'b', 1);
  T("aabb", 'b', 2);
  T("aaabb", 'b', 3);
  T("aaaabb", 'b', 4);
  T("aaaaabb", 'b', 5);
  T("aaaaaabb", 'b', 6);
  T("abc abc", 'c', 2);
  T(s, 1, 0);
  T(s, 2, 1);
  T(s, 10, 9);
  T(s, 11, 10);
  T(s, 127, 126);
  T(s, 128, 127);
  T(s, 255, 254);
  T(s, 0, 255);

  return 0;
}
