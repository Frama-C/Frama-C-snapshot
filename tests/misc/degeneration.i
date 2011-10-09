

void main(char c) {
  int **p;
  int * pp = *p;
  int ppp = **p;
  int pppp = ppp;
  int *qq = (c?&ppp:&pppp);
  int qqq = *qq;

  int q = **p+1;
  **p=1;
}
