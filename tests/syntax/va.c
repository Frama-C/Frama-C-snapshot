#include "stdarg.h"
#include "stdio.h"

// error locals have same scope
void h () {
  int x = 1;
  int x = 2;
}

// error: formal x and local x have the same scope, hence can't have the
// same name.
void g(int x) {
  int x = 1;
}


// error: formal x is shadowed by local x, hence can't be seen by va_start
void f(int x, ...) {
  { int x,y;
    va_list p;
    va_start(p,x);
    vscanf("FOO %d %d",p);
  }
}
