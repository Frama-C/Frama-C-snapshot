/* run.config
   STDOPT:
*/

#include "glob.c"
#include <stdio.h>
#include <string.h>


volatile int v;
int globerr0(const char *epath, int eerrno) {
#ifndef __FRAMAC__
  fprintf(stderr, "%s: %s\n", epath, strerror(eerrno));
#endif
  return 0;
}

int globerr1(const char *epath, int eerrno) {
#ifndef __FRAMAC__
  fprintf(stderr, "%s: %s\n", epath, strerror(eerrno));
#endif
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
