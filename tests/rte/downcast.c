/* run.config
   OPT: -rte -rte-print -machdep x86_32 -journal-disable
   OPT: -rte -rte-no-all -rte-print -rte-signed -machdep x86_32 -journal-disable
   OPT: -rte -rte-no-all -rte-print -rte-unsigned-downcast -machdep x86_32 -journal-disable
*/
int main(void) {
  signed char sx,sy,sz;
  unsigned char uc;
  int x;
  unsigned int ux, uy,uz;
  unsigned short s;

  sz = sx + sy;

  uc = sx + sy;

  uc = x;

  x = uy + uz;

  ux = uy + uz;

  s = uy + uz;

  return 0;
}
