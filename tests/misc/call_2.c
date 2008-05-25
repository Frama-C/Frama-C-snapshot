int G = 0;
int H;

int f (int x) {
  G = x;
  return 0;
}

int i_auCyc () {
  f(0);
  G=17;
  if (H) f(2);
  else f(5);
  f(6);
  return 0;
}

extern void fp (int*p);

int G,x;
void main() {
  x = 1;
  G = 0;
  while (x) {
    G ++;
    }


}

