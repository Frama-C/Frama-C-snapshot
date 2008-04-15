/* run.config
   DONTRUN: cast no working yet
*/

struct int_struct {
  char c1;  
  char c2; 
  char c3;  
  char c4;
};

/*@ requires \valid(i);
  @ ensures *i == 0;
  @*/
void f(int* i) {
  struct int_struct* s = (struct int_struct*) i;
  s->c1 = 0;
  s->c2 = 0;
  s->c3 = 0;
  s->c4 = 0;
}

// take address of quantified variable in axiom
//@ axiom c1_zero{L}: \forall int i; i == 0 ==> ((struct int_struct*)&i)->c1 == 0;
//@ axiom c2_zero{L}: \forall int i; i == 0 ==> ((struct int_struct*)&i)->c2 == 0;

//@ axiom c3_zero{L}: \forall int* i; *i == 0 ==> ((struct int_struct*)i)->c3 == 0;
//@ axiom c4_zero{L}: \forall int* i; *i == 0 ==> ((struct int_struct*)i)->c4 == 0;

//@ requires \valid(i);
void g(int* i) {
  *i = 0;
  struct int_struct* s = (struct int_struct*) i;
  //@ assert s->c1 == 0;
  //@ assert s->c2 == 0;
  //@ assert s->c3 == 0;
  //@ assert s->c4 == 0;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make cast_same_size"
End:
*/
