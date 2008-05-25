/* run.config
   GCC:
   OPT: -security-slicing -lib-entry f -slicing-level 3 -slice-print
   OPT: -security-slicing -lib-entry f1 -slicing-level 3 -slice-print
   OPT: -security-slicing -lib-entry f2 -slicing-level 3 -slice-print
   OPT: -security-slicing -lib-entry f3 -slicing-level 3 -slice-print
   OPT: -security-slicing -lib-entry f -slicing-level 3 -security-lattice strong -slice-print
*/

void do_something(int a, int b);

/*@ ensures \result == -a; */
int inv(int a);

/*@ requires security_status(s) == public(); */
void send(int s);

int a, b;
int x, y, r, toto;

void g(int x, int y, int *z) {
  r = x;
  b = 2 * y;
  *z = 0;
}

void f(void) {
  int z;
  if (b > a) do_something(toto,y); else g(x,y,&z);
  if (b > a) { z = inv(b); send(z); } else { g(x,y,&z); send(z); }
  if (b) do_something(toto,toto);
  if (b) { g(x,y,&z); send(z); }
}

/* ************************************************************************** */

void f1(void) {
  int z;
  if (b > a) do_something(toto,y); else g(x,y,&z);
  if (b > a) { z = inv(b); send(z); } else { g(x,y,&z); }
  if (b) do_something(toto,toto);
  if (b) { g(x,y,&z); }
}

/* ************************************************************************** */

void f2(void) {
  int z;
  if (b > a) do_something(toto,y); else g(x,y,&z);
  if (b > a) { z = inv(b); } else { g(x,y,&z); send(z); }
  if (b) do_something(toto,toto);
  if (b) { g(x,y,&z); }
}

/* ************************************************************************** */

void f3(void) {
  int z;
  if (b > a) do_something(toto,y); else g(x,y,&z);
  if (b > a) { z = inv(b); } else { g(x,y,&z); }
  if (b) do_something(toto,toto);
  if (b) { g(x,y,&z); send(z); }
}
