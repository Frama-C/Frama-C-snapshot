/* run.config
DONTRUN: main test is in extern_init.i
*/
extern int a[];

void f();
void g();

int main() {
  f();
  g();
  return a[1];
}
