void titi() {
  int c = 0;
    L0: ;
    /* ill-formed: in ghost mode, we have two local c in the same scope. */
    /*@ ghost int c = 1; */
    L1: ;
    c = 2;
    /*@ assert c == 1; */
    /*@ assert \at(c,L0) == 0; */
    /*@ assert \at(c,L1) == 1; */
  /*@ assert c == 2; */
}

void toto () {
  //@ ghost int c = 0;
  // ill-formed: the instruction should be ghost as well
  c++;
}
