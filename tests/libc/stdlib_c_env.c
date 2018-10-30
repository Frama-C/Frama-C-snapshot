#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "stdlib.c"

int main() {
  char *env0 = "BLA=1";
  int i1 = putenv(env0);
  char s[10] = "NAME=val";
  int i2 = putenv(s);
  char *r1 = getenv("BLA");
  if (r1) {
    //@ assert valid_read_string(r1);
  }
  strcpy(s, "BLE=val");
  char *r2 = getenv("BLA");
  int i3 = setenv("BLA", "val", 0);
  int i4 = setenv(r2, "val", 1);
  int i5 = setenv(r2, r2, 0);
  int i6 = unsetenv("BLE");
  //@ assert i6 == 0; // does not contain '='
  int i7 = unsetenv("BLE=");
  //@ assert i7 == -1; // contains '='
  int i8 = unsetenv(r2);
  char *r3 = getenv(r2);
  return 0;
}
