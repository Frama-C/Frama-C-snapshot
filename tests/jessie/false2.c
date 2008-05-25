
int e;

/*@ assigns e;
  @ ensures e == 0;
  @*/
void f(void);

/*@ ensures e == 1;
  @*/
int main() {
  e=1;
  f();
  return 0;
}

/*
Local Variables:
compile-command: "make false2"
End:
*/
