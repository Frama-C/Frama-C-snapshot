/* run.config
   GCC:
   STDOPT: +"-impact-pragma impact" +"-lib-entry" +"-main impact" +"-impact-slicing" +"-then-on 'impact slicing'" +"-print"
   */

int a, b, c, e, x, y, z, f, w;

void impact() {
  if (c) a = 18; else x = 5;
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
