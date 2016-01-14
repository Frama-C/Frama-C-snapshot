/* run.config
   STDOPT: +"-val-subdivide-non-linear 14 -value-msg-key nonlin"
*/

volatile int v;

void main() {
  int y;
  short z = v;
  int k = (z+675) * (z+675);
  int l = (z+17817) * (z+17817);

  int x = sizeof(y)+sizeof(y); // do not optimize y
  int *p = &x + x; // do not optmize x;
}
