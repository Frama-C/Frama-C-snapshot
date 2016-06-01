/* run.config
   OPT: -rte -then -print
   OPT: -warn-unsigned-overflow -rte -then -print
 */

unsigned int f(unsigned int a, unsigned int b) {
  unsigned int x, y, z;
  x = a << 3;
  y = b * (unsigned int )2;
  z = x - y;
  return (z);
}
