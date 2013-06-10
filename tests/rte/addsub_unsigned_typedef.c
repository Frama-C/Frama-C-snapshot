/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32
   OPT: -rte -warn-signed-overflow -warn-unsigned-overflow -print -machdep x86_32 
*/

typedef unsigned int uint;

int main() {
  
  uint ux,uy,uz;

  ux = 0x7FFFFFFFU * 2 ; /* no unsigned ov */

  uy = 0x80000000U +  0x80000000U; /* unsigned ov */

  uy = 2U * 0x80000000U; /* unsigned ov */

  uz = ux + 2; /* unsigned ov but not detected by const folding */

  return 0;
}
