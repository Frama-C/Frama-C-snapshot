
f(int *p){
  p[1]=12;
}

int g(int x){
  int t[2];
  f(t);
}

main(){
  int lmain[2];
  f(lmain);
  g(2);
}  
