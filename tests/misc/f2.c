/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main f
*/
int f(int x) {
/* Here we are */
/*@ loop pragma UNROLL_LOOP 10; */
  while(1) { return 0 ;}
  return 2;
}
