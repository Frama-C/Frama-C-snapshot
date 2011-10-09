#include "share/libc/stddef.h"

int main() {

  // String literals are lvalues
  char (*p)[4] = &("bar");
  wchar_t (*q)[4] = &(L"foO");

  if((*p)[1] != 'a') return 1;
  if((*q)[1] != 'o') return 2;
  
  return 0;
}
