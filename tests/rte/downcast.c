/* run.config
   OPT: -rte -warn-signed-overflow  -warn-signed-downcast -print -machdep x86_32
   OPT: -rte -warn-signed-overflow -rte-no-all -print -warn-signed-overflow -machdep x86_32
   OPT: -rte -warn-signed-overflow -rte-no-all -print -warn-signed-downcast -warn-unsigned-downcast -machdep x86_32
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
