/* run.config
   OPT: -rte -warn-signed-overflow  -warn-signed-downcast -print -machdep x86_32
*/
int main() {
  
  int x=0,y=0,z=0;
  unsigned int ux=0,uy=0,uz=0;
  short sz=0;

  z = -x;
  z = - (-0x7fffffff -1);
  z = -ux;
  
  sz = ((unsigned short) (65535 + 3)) + x;

  z = -0x80000000 -1; /* this is  unsigned and equal to 0x7fffffff */
  z = -2147483648 - 1; /* this is unsigned and equal to 0x7fffffff */ 
  z = -2147483647 -1 -1; /* this is signed and overflows */
  z = -((int)(-0x7fffffff -1)) -1; /* this is signed and overflows */

  return 0;
}
