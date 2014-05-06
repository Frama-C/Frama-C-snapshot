/* run.config
   STDOPT: +"-cpp-command \"gcc -C -E -I. -Wno-comment\""
 */

/*@ ensures "/*"[0] == '/'; */
char f(void) { return "/*"[1]; }
