
int e;

/*@ assigns e;
  @ ensures e == 2;
  @*/
void f() {
e=2;
}

/*@ assigns e;
  @ ensures e == 6;
  @*/

int main() {
e=1;
f();
return 0;
}
