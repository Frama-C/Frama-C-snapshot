/* run.config
   OPT: -rte -then -print
   OPT: -warn-signed-downcast -rte -then -print
 */

int main(void) {
  signed char cx, cy, cz;

  cz = cx + cy;
  return 0;
}
