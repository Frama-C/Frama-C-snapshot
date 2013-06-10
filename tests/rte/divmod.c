/* run.config
   OPT: -rte -warn-signed-overflow -warn-signed-downcast -print -machdep x86_32 -journal-disable
*/

#include "share/libc/limits.h"


int main() {
  
  int x=0,y=0,z=0;
  unsigned int ux=0,uy=0,uz=0;

  z = INT_MIN / -1 ;
  z = INT_MIN % -1 ;

  uz = 1 / 0;
  uz = 1 / (0xffffffff + 1);

  ux = 0x80000000;
  uy = 0xffffffff;

  uz = ((int) ux) / ((int) uy); // floating point exception
  uz = ux / uy; // correct if uy != 0
  uz = 0x80000000 / (0xffffffff + 1);
  uz = ((int) (-0x7fffffff -1)) / ((int) -1);
  uz = ((int) (-0x7fffffff -1)) /  0xffffffff;
  uz =  0x80000000 / (int) -1;
  uz = (int) (0x80000000 / 0xffffffff);

  z = 1 / (x + y) ;

  z = x / -1;
  
  z = (- 0x7ffffff - 1) / y;

  z = (-2147483648L) / (-1L) ;

  z = 0x80000000 / -1;

  z = 0x80000000 / 0xffffffff;

  return 0;
}
