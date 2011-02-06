
void test_bw_and() {
  //@ assert (3&1) == 1;
}

//@ ensures \result == ~1;
int main()
{
  char  c1 = 1, c2 = 2;
  char  c3,c4,c5,c6,c7,c8;
  c3 = c1 & c2;
  //@ assert c3 == 0 ; 
  c4 = c1 | c2;
  //@ assert c4 == 3 ;
  c5 = c1 ^ c2;
  //@ assert c5 == 3 ;
  c6 = ~c1;
  //@ assert c6 == 254 ; 
  c7 = c1<<2;
  //@ assert c7 == 4 ; 
  c8 = c1>>2;
  //@ assert c8 == 0;
  return (1^-1); 
}



