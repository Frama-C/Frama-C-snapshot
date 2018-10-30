/* run.config
   DONTRUN: main test at used-files.i
*/

int leaf(void);
int f(void);

int g() {
  return f();
}
