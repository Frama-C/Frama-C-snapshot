int main(){
  {
    int a ;
  }
  ; // < NOP inserted
}

void f() {
  if (0) {
    int b;
  }
}
