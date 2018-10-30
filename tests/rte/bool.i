/* run.config
OPT: -warn-invalid-bool -rte -print
*/

struct s_bool { char c; _Bool b; } sb;

_Bool ko1 () {
  char *p = &sb.c;
  *(p+1) = 17;
  return sb.b;
}

_Bool ko2 () {
  _Bool b;
  char *p = (char *)&b;
  *p = 17;
  return b;
}

extern _Bool f(void) ;
_Bool g (void) {
  _Bool x = f() ;
  _Bool y = ko2();
  if (x) return y;
  return x;
}
