/* run.config
   OPT:  -journal-disable -wp -wp-model Caveat -wp-proof none
*/

/*
  model name : Caveat
  kind : Positive
  bhv : first part Provable; last part not yet finished
*/

void var_subst_const (void) {
  int x = 3;
    //@ assert x == 3;
}

void var_subst_var (int x) {
  int y = x + 1;
  //@ assert y == x + 1;
  x = x + 1;
  //@ assert y == x;
}
void assign_ptr (void) {
  int x;
  int * p = &x;
  //@ assert p == &x;
}

void ptr_subst (int * p) {
  *p = 3;
  //@ assert *p == 3;
}
void ptr_on_var (void) {
  int x = 0;
  int * p = &x;
  //@ assert *p == 0;
}
  
void read_ptr (int * p) {
  int y = *p;
  //@ assert y == *p;
}

void read_ptr_shift (int * p) {
  int i = 3;
  int y = *(p + i);
  //@ assert y == *(p+3);
}

int main (void) { return 0 ; }
