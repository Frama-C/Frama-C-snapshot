#include <stdio.h>
#include <wchar.h>
volatile int v;
int main() {
  FILE *fd = fopen("bla", "r");
  if (!fd) return 1;
  wchar_t buf[30];
  wchar_t *res = fgetws(buf, 29, fd);
  if (!res) return 1;
  //@ assert res == buf;
  wchar_t buf2[2];
  buf2[0] = L'a';
  wchar_t *r = wmemchr(buf2, L'a', 2); // no warning
  //@ check ok: r != \null;
  r = wmemchr(0, 0, 0); // should be ok
  //@ check ok: r == \null;
  if (v) {
    r = wmemchr(buf2, 0, 2); // red alarm (uninit)
    //@ assert unreachable:\false;
  }
  r = wmemchr(buf2, L'a', 3); // no warning
  //@ check ok: r != \null;
  if (v) buf2[1] = L'b';
  r = wmemchr(buf2, L'a', 3); // no warning
  //@ check ok: r != \null;
  r = wmemchr(buf2, L'b', 3); // warning: buf2[1] maybe uninit
  //@ check ok: r != \null;
  buf2[1] = L'b';
  r = wmemchr(buf2, L'b', 3); // no warning
  //@ check ok: r != \null;
  wchar_t *wsrc = L"wide thing";
  wchar_t wdst[10];
  r = wcsncpy(wdst, wsrc, 10); // no warning
  //@ check ok: r == wdst;
  //@ check ok: \initialized(&wdst[9]);
  if (v) {
    r = wcsncpy(wdst, wsrc, wcslen(wsrc)+1); // error: not enough room
    //@ assert unreachable:\false;
  }
  if (v) {
    wcsncpy(wdst, wdst, 10); // error: no separation
    //@ assert unreachable:\false;
  }
  if (v) {
    wcsncpy(0, wsrc, 10); // error: invalid dest
    //@ assert unreachable:\false;
  }
  if (v) {
    wcsncpy(wdst, 0, 10); // error: invalid src
    //@ assert unreachable:\false;
  }
  if (v) {
    wcsncpy(wsrc, wdst, 10); // error: non-writable dest
    //@ assert unreachable:\false;
  }
  wcsncmp(wsrc, wsrc, 11); // no warning
  wcsncmp(wsrc, wdst, 11); // warning: wdst possibly invalid
  wchar_t wdst2[20] = {0};
  wcsncat(wdst2, wsrc, 11); // no warning
  wcsncat(wdst2, wsrc, 10); // no warning (if wdst2 is precise)
  //@ loop unroll 10;
  for (int i = 0; i < 10; i++)
    wdst2[i] = L'A';
  wdst2[10] = L'\0'; // wdst2 now has length 10
  if (v) {
    wcsncat(wdst2+10, wdst2, 10); // error: no separation
    //@ assert unreachable:\false;
  }
  return 0;
}
