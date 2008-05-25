int G;

//@ ensures G == 6;
void main () {
  G = 6;
}

//@ ensures G == 7;
void main1 () {
  G = 6;
}

int *p;

//@ ensures *p == 6 && G == *p && G == 6;
void main2 () {
  p = &G;
  *p = 6;
}

typedef struct {
  int a;
  int b;
  int c;
} st;

st TAB[10];

//@ ensures TAB->a == 12;
void main3 () {
  TAB->a = 12;
}
