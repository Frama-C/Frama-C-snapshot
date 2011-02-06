/* run.config
   OPT: tests/cil/cpu_a.c -machdep x86_16 -print
*/

typedef unsigned int DWORD ;

DWORD f(void);

DWORD g(void) { return f(); }
