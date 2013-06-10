#include "share/libc/stddef.h"

int main(volatile int v) {

  // String literals are lvalues
  char (*p)[4] = &("bar");
  wchar_t (*q)[4] = &(L"foO");

  if((*p)[1] != 'a') return 1;
  if((*q)[1] != 'o') return 2;

  if((*p)[3] != 0) return 3;
  if((*q)[3] != 0) return 4;

  if (v) {
    char c = (*p)[4];
  }
  if (v) {
    wchar_t wc = (*q)[4];
  }
  
  return 0;
}
