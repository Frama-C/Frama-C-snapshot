
int b;
int t[10];

/*@ requires b == 0 ;
  @ ensures b == 1;
  @*/
void f() {
  t[b++] = 1;
}

/*
Local Variables:
compile-command: "make incr"
End:
*/
