/* run.config
   STDOPT: #"@EVA_OPTIONS@ -calldeps -then -impact-pragma g"
   */

int a, b, c, d, e;

void f() {
  if (a)
    c = d; // should not be selected
  else
    b = e;
}

void g() {
  //@ impact pragma stmt;
  d = 2;
  e = d;
  f();
}

void main () {
  a = 1;
  f();
  a = 0;
  g();
}

