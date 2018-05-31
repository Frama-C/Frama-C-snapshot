/* run.config
   STDOPT: #"-slevel 10 -val-split-return full"
*/

#include "glob.c"
#include <stdio.h>
#include <string.h>
#include "string.c"

volatile int v;
int globerr0(const char *epath, int eerrno) {
  fprintf(stderr, "%s: %s\n", epath, strerror(eerrno));
  return 0;
}

int globerr1(const char *epath, int eerrno) {
  fprintf(stderr, "%s: %s\n", epath, strerror(eerrno));
  return 1;
}

int main() {
  int ret, flags;
  glob_t gl;
  gl.gl_offs = 1;
  flags = GLOB_DOOFFS;
  ret = glob("bla", flags, globerr0, &gl);
  if (gl.gl_pathc > 0) globfree(&gl);
  flags = GLOB_ERR;
  ret = glob("ble", flags, globerr0, &gl);
  if (gl.gl_pathc > 0) globfree(&gl);
  flags = GLOB_NOCHECK;
  ret = glob("bli", flags, globerr1, &gl);
  if (gl.gl_pathc > 0) globfree(&gl);
  gl.gl_offs = 1;
  flags = GLOB_DOOFFS | GLOB_NOCHECK;
  ret = glob("blo", flags, globerr1, &gl);
  if (gl.gl_pathc > 0) globfree(&gl);
  return 0;
}
