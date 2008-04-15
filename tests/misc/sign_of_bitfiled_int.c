/* run.config
   OPT: -val -cpp-command "gcc -C -E -DPTEST"  -journal-disable
   OPT: -machdep ppc_32 -val -cpp-command "gcc -C -E -DPTEST" -journal-disable
*/


#ifndef PTEST
#include <stdio.h>
#endif

struct sbf { int c:16 ; unsigned int u:16 ;} bf ;
int main () {
  int int_inside_bitfield_is_unsigned ;
  bf.u --;
  bf.c --;
  int_inside_bitfield_is_unsigned = (bf.u > bf.c) ;

#ifndef PTEST
  printf("int_inside_bitfield_is_unsigned = %d\n",
         int_inside_bitfield_is_unsigned);
#endif
  return int_inside_bitfield_is_unsigned ;

}
