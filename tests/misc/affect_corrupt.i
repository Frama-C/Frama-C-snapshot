/* run.config
   GCC:
   OPT: -val -deps -out -input -journal-disable
   OPT: -val -deps -out -input -absolute-valid-range 0-0x3 -journal-disable
*/
int *p,r=77;
void main () {
  r = *p;
}
