/* run.config
   STDOPT: +"-cpp-command='gcc -C -E -I.'"
   STDOPT: +"-cpp-command='gcc -C -E -I. -DERR'"
 */

int t[4];

#ifdef ERR
int q[static 3];
#endif

void f(int a[static 3]) {
  a[2] = 3;
}

int main () {
  f(t);
  return 0;
}
