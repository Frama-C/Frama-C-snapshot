/* Simplification do {...} while(0) into {...}. 
*/
void f1() {
  //@ loop invariant \true;
  do {
    int x = 1;
    int y = 2;
  } while(0);
}

void f2() {
  do {
    int x = 1;
    int y = 2;
    break;
  } while(0);
}


void f3() {
  do {
    int x = 1;
    int y = 2;
    continue;
  } while(0);
}

void f4(int c) {
  do {
    int x = 1;
    int y = 2;
  } while(c);
}

/* Functions below are simplified */

void f5() {
  do {
    int x = 1;
    int y = 2;
  } while(0);
}

void f6() {
  do {
    int x = 1;
    int y = 2;
    while (1) {
      continue;
      break;
    }
  } while(0);
}

void f7() {
  do {
    int x = 1;
    int y = 2;
  } while((int *)0);
}

void f8(int v) {
  if (v == 0) {
    do {} while (0);
  }
}
