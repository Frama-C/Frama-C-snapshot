/* run.config
 OPT: -check -slice-return main -journal-disable -then-on 'Slicing export' -print
 OPT: -check -slice-pragma main -journal-disable -then-on 'Slicing export' -print
 OPT: -check -sparecode-analysis -journal-disable
*/

struct {int a; int ab; int b; int c ; int d;} S;
int X, Y;
void g (void) {
  S.a = 1;
  S.ab = 0;
  S.b = 2; /* here, better can be done ! */
  S.d = 4;
}
int main (void) {
  g();
  //@  slice pragma expr S.b;
  S.ab = 1; /* so that S.ab is sparecode in g() */
  return S.a ;
}
