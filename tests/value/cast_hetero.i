int X;
int*pt;
void f(int c) {
  pt = &X;
  *pt = c;
}

int T[10]={0};

void g(int c){
  pt = &X;
  T[X] = c;
}

void main() {
  g(1);
}
