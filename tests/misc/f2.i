/* run.config
  GCC:
  OPT: -val -deps -out -input  -main f -journal-disable
*/
int f(int x) {
/* Here we are */
/*@ loop pragma UNROLL 10; */
  while(1) { return 0 ;}
  return 2;
}
