/* run.config
   OPT: -rte -rte-float-to-int -warn-special-float none -then -print
 */

int f(float v) {
  int i = (int)(v+3.0f);
  return i;
}
