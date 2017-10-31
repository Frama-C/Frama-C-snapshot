char T[] = { 1,0,0,0,1,2,3,4,5,0,1,1,1 } ;

volatile int nondet;

union u {
  int i;
  short s[2];
};

/* This function tests the join of two offsetmaps with the same bitwise
   representation, but different structured values. Ideally, the resulting
   offsetmap should be a singleton. */
void join_offsetmap () {
  union u u;
  if (nondet)
    u.i = -2;
  else {
    u.s[0] = -2; u.s[1] = -1;
  }
  int r = u.i;
}

int merge_bits () {
  Frama_C_show_each_F(*((int*)(T)));
  Frama_C_show_each_F(*((int*)(T+1)));
  Frama_C_show_each_F(*((int*)(T+4)));
  Frama_C_show_each_F(*((int*)(T+9)));
  *((int*)(T+2))=2U<<31 | 2U << 30 | 2U << 27 | 2U << 3;
  Frama_C_show_each_F(*((int*)(T)));
  return 0;
}


int main () {
  join_offsetmap ();
  merge_bits ();
}
