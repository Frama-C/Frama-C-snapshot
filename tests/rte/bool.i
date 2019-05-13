/* run.config
OPT: -warn-invalid-bool -rte -print -then -rte-trivial-annotations -rte -print
*/

/* The test asks for two executions of RTE plug-in:
   - a first one without trivial annotations (default behavior)
   - a second one with trivial annotations (-rte-trivial-annotations)
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
/* There is no rte in 'ok1' statements (needs -rte-no-trivial-annotations).
   If there is some during its execution,
   that is into the statements of the called functions.
*/
_Bool ok1 (void) {
  _Bool x = f() ;
  _Bool y = ko2();
  if (x) return y;
  return x;
}

/* There is no rte when converting to _Bool (needs -rte-no-trivial-annotations),
   nor for access to local variables or formal parameters
   when their address is not taken.
*/
extern int g(_Bool,_Bool) ;
_Bool ok2 (int a,_Bool b) {
  return g(a>0, b);
}
