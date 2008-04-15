/* run.config
   OPT: -security-slicing -slice-print -security-lattice strong -lib-entry -journal-disable
*/

/*@ requires security_status(s) == public; */
void send(const int s);

int V1,V2,V3,V4,V5;

void main() {
  int a,b,c,d,e;
  int x,y,z,t;
  y = V1;
  e = y;
  b = V2;
  c = V3;
  x = V4;
  y = V5;
  a = x;
  if (c) { y = x; a = 1; }
  z = y;
  send(z);
  t = e;
  if (a) { b = 2; }
  d = a + b;
}
