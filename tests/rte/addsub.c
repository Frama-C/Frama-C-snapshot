/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32
*/

int main() {
  
  int x=0,y=0,z=0;

  z = (int) 0x7fffffff + (int) 0x7fffffff; /* false */
  z =  - 0x7fffffff - 0x7fffffff; /* false */
  z = (- (int) (-0x7fffffff -1)) - 1; /* false */

  z = (int) 0x7fffffff + 0; /* true */
  z = - (int) 0x7fffffff - 1; /* true */

  z = x + y;

  z = - (int) 0x7ffffffc - y;
  z = - x - (int) 0x7ffffffc;

  z = (int) 0x7ffffffc + y;
  z = x + (int) 0x7ffffffc;


  return 0;
}
