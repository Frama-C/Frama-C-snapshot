struct {
  int a ;
  struct {
    int gcc_a ;
    int gcc_b ;
  } ;
  int b ;
} Sa ;

//@ ensures Sa.gcc_a == Sa.a && Sa.gcc_b == Sa.b;
void set_anonymous_struct (void) {
  Sa.gcc_a = Sa.a ;
  Sa.gcc_b = Sa.b ;
}

int main () {
  Sa.a = 42;
  Sa.b = 3;
  set_anonymous_struct();
  return 0;
}
