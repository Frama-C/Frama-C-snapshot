/* run.config
   GCC:
   STDOPT: +"-impact-pragma loop" +"-lib-entry" +"-main loop"
   */

int c,x,y,z,w;

void loop () {
  while (c) {
    z = w + 1;
    z = y + 1;
    /*@ impact pragma stmt; */
    x = x + 1;
    y = x + 1;
  }
  w = z;
}
