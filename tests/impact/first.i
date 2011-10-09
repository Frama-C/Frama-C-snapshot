/* run.config
   GCC:
   STDOPT: +"-impact-pragma impact" +"-lib-entry" +"-main impact"
   */

int a, b, c, e, x, y, z, f, w;

void impact() {
  /*@ impact pragma stmt; */
  b = a;
  if (c) {
    x = b + c;
    y = x + e;
  } else
    z = 12;
  z = 13;
  z = y + f;
  w = b;
}
