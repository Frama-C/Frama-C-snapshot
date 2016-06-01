/* Test that we do not perform a merge on return instructions, even if there
   is insufficient slevel. */

int x;
int y;

//@ ensures x < 0 || y == x + 1;
void main1(int c) {
  if (c == 1) {
    x = 0;
    y = 1;
    return;
  } else if (c == 2) {
    x = 5;
    y = 6;
    return;
  } else {
    x = -3;
    return;
  }  
}

//@ ensures x < 0 || y == x + 1;
void main2(int c) {
  if (c == 1) {
    x = 0;
    y = 1;
    return;
  }
  
  if (c == 2) {
    x = 5;
    y = 6;
    return;
  }

  x = -3;
  Frama_C_dump_each();  
}

void main3();

void main(int c) {
  main1(c);
  main2(c);
}
