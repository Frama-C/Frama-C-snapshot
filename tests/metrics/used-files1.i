/* run.config
   DONTRUN: main test at used-files.i
*/

int g(void);
int unused_g(void);

int h() {
  return g();
}

int unused_h() {
  return unused_g();
}
