/* run.config
   DONTRUN: main test at used-files.i
*/

int leaf(void);
void indirect_in_gvar_init(void);

int f() {
  void (*ifp)(void) = indirect_in_gvar_init;
  return leaf();
}
