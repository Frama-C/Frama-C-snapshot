int *p;

/*@ requires \valid(p);
  @ ensures \valid(p);
  @*/
void f(int *q) {}

/*@ requires \valid(p);
  @ ensures \valid(p);
  @*/
void g()
{
 int i;

 f(&i);
}
