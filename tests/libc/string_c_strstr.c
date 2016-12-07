/* run.config
   STDOPT: #"-cpp-extra-args=-includeshare/libc/string.c -slevel-function strstr:30"
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

#define N(s, sub) {                                                     \
    char *p = s;                                                        \
    char *q = strstr(p, sub);                                           \
    if (q)                                                              \
      t_error("strstr(%s,%s) returned str+%d, wanted 0\n", #s, #sub, q-p); \
  }

#define T(s, sub, n) {                                                  \
    char *p = s;                                                        \
    char *q = strstr(p, sub);                                           \
    if (q == 0)                                                         \
      t_error("strstr(%s,%s) returned 0, wanted str+%d\n", #s, #sub, n); \
    else if (q - p != n)                                                \
      t_error("strstr(%s,%s) returned str+%d, wanted str+%d\n", #s, #sub, q-p, n); \
  }

int main(void)
{
  N("", "a");
  N("a", "aa");
  N("a", "b");
  N("aa", "ab");
  N("aa", "aaa");
  N("abba", "aba");
  N("abc abc", "abcd");
  N("0-1-2-3-4-5-6-7-8-9", "-3-4-56-7-8-");
  N("0-1-2-3-4-5-6-7-8-9", "-3-4-5+6-7-8-");
  N("_ _ _\xff_ _ _", "_\x7f_");
  N("_ _ _\x7f_ _ _", "_\xff_");

  T("", "", 0);
  T("abcd", "", 0);
  T("abcd", "a", 0);
  T("abcd", "b", 1);
  T("abcd", "c", 2);
  T("abcd", "d", 3);
  T("abcd", "ab", 0);
  T("abcd", "bc", 1);
  T("abcd", "cd", 2);
  T("ababa", "baba", 1);
  T("ababab", "babab", 1);
  T("abababa", "bababa", 1);
  T("abababab", "bababab", 1);
  T("ababababa", "babababa", 1);
  T("abbababab", "bababa", 2);
  T("abbababab", "ababab", 3);
  T("abacabcabcab", "abcabcab", 4);
  T("nanabanabanana", "aba", 3);
  T("nanabanabanana", "ban", 4);
  T("nanabanabanana", "anab", 1);
  T("nanabanabanana", "banana", 8);
  T("_ _\xff_ _", "_\xff_", 2);

  return 0;
}
