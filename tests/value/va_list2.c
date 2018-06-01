/*run.config*
  STDOPT:
  STDOPT: #"-variadic-no-translation"
*/
#include "stdarg.h"


void main(const char* fmt, ...) {
  va_list args;

  va_start(args, fmt);
  while (*fmt) {
    switch(*fmt) {
    case 1: {
      int i = va_arg(args, int);
      Frama_C_show_each_i(i);
      break;
    }
    case 2: {
      float f = va_arg(args, float);
      Frama_C_show_each_f(f);
      break;
    }
      //default:
    }
    fmt++;
  }
  va_end(args);
}
