/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -absolute-valid-range 0-0x3
*/

char R;
void main() {
  *((char*)0)=2;
  R = *((char*)1);
  *((char*)2)=2;
  R = *((char*)3);

}
