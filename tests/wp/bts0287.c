
int s ;
//@ assigns s;
void g(int x);
//@ assigns s ; //<- doit être prouvable
void f(void) {
  g(1);
}
int main (void) {return 0;}
