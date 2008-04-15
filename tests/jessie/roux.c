int *p;

/*@ requires \valid(q);
  @ assigns *q;
  @*/
void f(int *q) {*q = 42;}

/*@ requires \valid(p) && *p == 0;
  @ ensures *p == 0;
  @*/
void g()
{
 int i[2];

 /*
     assert { valid(int_P_alloc_table, p) };
     assert { not in_pset(p, pset_singleton(shift(i_6,1))) };
  */

 assert (p != i);
 f(&i+1);
}
