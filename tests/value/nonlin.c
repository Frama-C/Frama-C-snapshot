/* run.config*
   STDOPT: +"-val-subdivide-non-linear 14 -value-msg-key nonlin"
*/

volatile int v; volatile short vs;

void main() {
  int y;
  short z = v;
  int k = (z+675) * (z+675);
  int l = (z+17817) * (z+17817);

  int x = sizeof(y)+sizeof(y); // do not optimize y
  int *p = &x + x; // do not optmize x;

  long long i1 = vs;
  long long i2 = vs;
  long long r = i1 * i1 + (i2+3) * (i2+3); // (i2+3) not fully precise with 14 subdivisions

  int t[102];
  short idx = vs;
  //@ assert 0 <= idx <= 10;
  t[idx*idx] = 1;
}
