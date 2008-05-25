/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input 
   OPT: -memory-footprint 1 -val -deps -out -input -absolute-valid-range 0-0x3
*/
int *p,r=77;
void main () {
  r = *p;
}
