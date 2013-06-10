/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32
*/

typedef int tint;

int main() {
  
  tint x=0,y=0,z=0;

  z = (tint) 0x7fffffff + (tint) 0x7fffffff; /* false */
  z =  - 0x7fffffff - 0x7fffffff; /* false */
  z = (- (tint) (-0x7fffffff -1)) - 1; /* false */

  z = (tint) 0x7fffffff + 0; /* true */
  z = - (tint) 0x7fffffff - 1; /* true */

  z = x + y;

  z = - (tint) 0x7ffffffc - y;
  z = - x - (tint) 0x7ffffffc;

  z = (tint) 0x7ffffffc + y;
  z = x + (tint) 0x7ffffffc;


  return 0;
}
