/* run.config
   OPT: -out -input
 */

int a, b;

void f() {
  a = b;
}

void g () {
  int x = 0;
  if (x) f ();
}

void main(){
  f ();
  g ();
}
