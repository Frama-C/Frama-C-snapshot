/*run.config
  OPT: -lib-entry -main main -val -journal-disable
 */
int G;

int f() {
  if (G) f();
  return 5;
}
void main() {
  G = f();
  return;
}
