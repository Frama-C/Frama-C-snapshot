/* run.config*
   STDOPT: +"-value-msg-key d-symblocs"
*/

volatile v;
int t[10]; extern u[10];

void main1() {
  unsigned int i = v;
  int k = v;

  t[i] = 3; 
  t[i] = t[i]+1; // The equality domain loses information here
  u[k] = t[i] + 2; Frama_C_dump_each();
  int j = t[i] + u[k]; 

  int *p = &t[i]; Frama_C_dump_each();
  int q = *p+1; // Does not write without adding something here, because otherwise we do a copy...

  if (u[i]+12 < 18) {
    Frama_C_dump_each();

    int iz = u[i]+11;
  }
}


void main2_kill_direct() {
  unsigned int i = v;  //@ assert i <= 8;

  t[i] = 4;
  Frama_C_dump_each();
  t[2] = 1;
  Frama_C_dump_each(); // t written, should be empty. Could be improved by
                       // detecting we write exactly in the location stored,
                       // and joining the current and previous value.
                       // Can be done syntactically on the lvalue
}

void main3_kill_indirect() {
  unsigned int i = v;  //@ assert i <= 8;

  t[i] = 4;
  Frama_C_dump_each();
  i = 8;
  Frama_C_dump_each(); // i written, should be empty
}


void main4_scope_right() {
  unsigned int i = v;  //@ assert i <= 8;
  {
    int x;
    t[i] = &x;
    Frama_C_dump_each();
  }
  Frama_C_dump_each(); // Should be empty, x out-of-scope
}

void main5_scope_lv() {
  int z = 1;
  {
    unsigned int i = v; //@ assert i <= 8;
    t[i] = z;
    Frama_C_dump_each();
  }
  Frama_C_dump_each(); // Should be empty, i out-of-scope
}

void main() {
  //  if (v) main1();
  if (v) main2_kill_direct();
  if (v) main3_kill_indirect();
  if (v) main4_scope_right();
  if (v) main5_scope_lv();
  Frama_C_dump_each(); // empty
}
