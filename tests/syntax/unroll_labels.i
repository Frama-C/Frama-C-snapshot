/* run.config
  STDOPT: +"-val"
  STDOPT: +"-val" +"-main main2" +"-slevel 3"
*/
enum { SIX = 6 } ;
volatile foo;

void main () {
  int j = 0;
  /*@ loop pragma UNROLL "completely", 4; */
  for (int i=1;i<4;i++) {
    switch (i) {
    case 1: j+=1; break;
    case 2: j+=3; break;
    case 3: j+=5; break;
    case 4: j+=7; break;
    default: j=0;
    }
  }

  {
    int x = 0;
  L:
    //@ loop pragma UNROLL 3;
    while(x<5) {
      int y=0;
      x++; y++;
    };
  }

  j = 0;
  //@ loop pragma UNROLL SIX;
  while(foo) {
    switch(j) {
    case -1: j++;
      break;
    case 0:
      //@ loop pragma UNROLL 3;
      while (j<5) {j++;}
      break;
    case 5:
      j = -1; break;
    default: return;
    }
  }

  {  
    if (j==0) goto zero;
    if (j==1) goto un;
    return;
  zero:
    //@ loop pragma UNROLL 3;
    while (j<5) { un: j++;}
  }

}

void main2 () {
  /*@ loop pragma UNROLL 2; */
  for (int i=0;i<2;i++) {
    for (int j=0;j<2;j++){
      i += 1;
      goto foo;
      i += 1;
    foo:
    }
  }
}

void main2_done () {
  /*@ loop pragma UNROLL 2;
    @ loop pragma UNROLL "done", 2; */
  for (int i=0;i<2;i++) {
    /*@ loop pragma UNROLL 2; */
    for (int j=0;j<2;j++){
      i += 1;
      goto foo;
      i += 1;
    foo:
    }
  }
}

void main3 (int c) {
  int i=0;
  if (c == 0) goto foo;
  /*@ loop pragma UNROLL 2; */
  for (;i<5;i++) {
    int j = 0 ;
    if (i == j) goto foo;
    if (i == 1) break;
    if (i == 2) continue;
    for (;j<5;j++){
      if (i == j) break;
      if (i < j) goto foo;
      if (i == j+1) continue;
      if (i == j+2) goto up;
      i += 1;
    foo:
      i += 1;
    }
  up:
  }
}

