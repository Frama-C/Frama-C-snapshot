/* run.config*
 OPT: -no-autoload-plugins -load-module from,inout,eva -eva-use-spec f,h -eva @EVA_CONFIG@ -inout -calldeps
 OPT: -no-autoload-plugins -load-module from,inout,eva -eva-use-spec f,h -eva @EVA_CONFIG@ -inout -calldeps -show-indirect-deps
*/


void f(int *x) {
}

void g(int *y);

//@ assigns *z \from \nothing;
void h(int *z) {
}

//@ assigns *w \from \nothing;
void i(int *w);

int w, x, y, z;

void main() {
  f(&x);
  g(&y);
  h(&z);
  i(&w);
}
