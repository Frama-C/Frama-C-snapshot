/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -lib-entry -main main
*/

char T[];

void main() {
  T[2]= 3;
  T[1] = T[3] +3;
}

