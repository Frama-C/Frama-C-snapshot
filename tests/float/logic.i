/* run.config
   STDOPT: +"-warn-decimal-float all" +"-float-hex"
*/

volatile v;

int main() {
  if (v) {
    double d = 0.1;
    //@ assert !(d == 0.1);
  }

  if (v) {
    double d = 0.1;
    //@ assert d == 0.1f;
  }

  if (v) {
    float f = 0.1;
    //@ assert !(f == 0.1);
  }

  // assert 0.1 == v;
}
