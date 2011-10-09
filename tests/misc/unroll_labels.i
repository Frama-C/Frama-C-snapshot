/* run.config
  OPT: -val -main main -check -print
  OPT: -val -main main2 -check -print -slevel 2
*/


void main () {
  int j = 0;
  /*@ loop pragma UNROLL_LOOP 4; */
  for (int i=1;i<4;i++) {
    switch (i) {
    case 1: j+=1; break;
    case 2: j+=3; break;
    case 3: j+=5; break;
    case 4: j+=7; break;
    default: j=0;
    }
  }
}

void main2 () {
  /*@ loop pragma UNROLL_LOOP 2; */
  for (int i=0;i<2;i++) {
    for (int j=0;j<2;j++){
      i += 1;
      goto foo;
      i += 1;
    foo:
    }
  }
}
