/* run.config
   OPT: -rte -rte-float-to-int -then -print
 */

int f(float v) {
  int i = (int)(v+3.0f);
  return i;
}
