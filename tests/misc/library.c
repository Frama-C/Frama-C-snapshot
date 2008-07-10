/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -lib-entry -main main -context-depth 3
*/
int f_int(int x);
int *f_star_int(int x);

int ****G;

int G0,*G1;
void main(void) {
  G0 = f_int(2);
  G1 = f_star_int(5);
  *G1 = 5;
  ****G=1;
}
