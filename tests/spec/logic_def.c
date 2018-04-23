/* run.config
    STDOPT: +"-val -value-verbose 2"
*/

//@ logic integer foo(int x) = x + 2 ;

int main() {
  int x = 42;
  //@ assert foo(x) >= x;
  return 0;
}
