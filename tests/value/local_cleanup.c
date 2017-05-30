int h() {
  int x = 1;
  {
    int y = 2;
    return y; // y must leave scope, even though the 'return' is not in the outermost scope
  }
}

void f(int *p){
  p[1]=12;
}

void g(int x){
  int t[2];
  f(t);
}

void main(){
  int lmain[2];
  f(lmain);
  g(2);
  h();
  Frama_C_dump_each();
}  
