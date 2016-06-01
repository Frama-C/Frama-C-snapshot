int R=77;
int G;
int* pG;
int F0;
int f0(int *p0) {
  F0 = R;
  *p0 = R;
  return R;
}

int F1;

int f1(int**pp1) {
  F1 = R;
  **pp1 = R;
  *pp1 = pG;
  f0(pG);
  return **pp1;
}

int H,XX;
int Fmain;

#pragma no_return ("Pre a : H==0;")
int main() {
  int *ph;
  int **pph;
  pG = &G;
  ph = &H;
  pph = &ph;
  Fmain = f1(pph);
  XX=0;
  return 0;
}

