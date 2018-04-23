#include <stdio.h>
#include <wchar.h>

int main() {
  FILE *fd = fopen("bla", "r");
  if (!fd) return 1;
  wchar_t buf[30];
  wchar_t *res = fgetws(buf, 29, fd);
  if (!res) return 1;
  //@ assert res == buf;
  return 0;
}
