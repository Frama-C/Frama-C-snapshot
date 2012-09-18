#include "../../share/libc.c"

int main (FILE * file) {
  int BUFFER[10]; size_t r;
  
  r = read(file,BUFFER,sizeof(BUFFER));
  r += BUFFER[3];
  return r+BUFFER[5];
  
}
