#include <stddef.h>

size_t fsize3(int n) {
  char b[n + 3]; // variable length array
  return sizeof b; // execution time sizeof
}

int main() {
  return fsize3(5);
}
