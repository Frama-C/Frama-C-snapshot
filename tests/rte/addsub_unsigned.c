/* run.config
   OPT: -rte -rte-print -machdep x86_32 -journal-disable
   OPT: -rte -rte-unsigned-ov -rte-print -machdep x86_32 -journal-disable 
*/

int main() {
  
  unsigned int ux,uy,uz;

  ux = 0x7FFFFFFFU * 2 ; /* no unsigned ov */

  uy = 0x80000000U +  0x80000000U; /* unsigned ov */

  uy = 2U * 0x80000000U; /* unsigned ov */

  uz = ux + 2; /* unsigned ov but not detected by const folding */

  return 0;
}
