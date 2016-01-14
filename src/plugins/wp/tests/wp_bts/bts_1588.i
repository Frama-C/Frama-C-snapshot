void f (int n, int *d ) {
  int i = 0, z = 0;

  //@ loop invariant l1_2: d == \at(d,Pre) + i;
  while (i < n)
  {
    ;
    //@ assert a1: d == \at(d,Pre) + i; 
    ;
    ;
    ;
    ;
    ;
    i++;
    d++;
    z++;
  }

  while (z != 0) {
    z--;
  }
}

