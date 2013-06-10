/* run.config
   OPT: -rte -warn-signed-overflow -warn-signed-downcast -print -machdep x86_32
*/

int main() {
  
  int x=0,y=0,z=0;
  unsigned int ux=0,uy=0,uz=0;

  uz = ux * uy;
  z = x * y;
  z = 0x1000 * y;
  z = x * 0x1000;
  z = (- 0x1000) * y;
  z = x * (- 0x1000);

  z = (int) (-1) * y;
  z = x * 1;
  z = 1 * y;
  z = x * 0xffffffff;

  z = 0xffff * 0xffff;
  z = 0xffff * 0x7fff;
  z = 0xffff * 0x8000;
  z = 0xffff * 0x8001;

  return 0;
}
