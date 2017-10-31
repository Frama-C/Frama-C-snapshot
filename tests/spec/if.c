
int a,b;

/*@ requires MyPre : (a<b)?\true:\false; */
void main(){ return ;}

//@ predicate P(char *s);
void g(char*s);

void f() {
  int x = 0;
  //@ assert P(x);
  g((char*)x);
} 
