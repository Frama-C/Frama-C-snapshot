/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main g
*/

extern int any_int(void);

void g() {
  int t;
  unsigned int G;
  t = any_int();
  G = t;
  t = t+1;

}
