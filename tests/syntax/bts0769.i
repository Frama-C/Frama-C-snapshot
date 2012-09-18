struct s {
  struct {int ui;} _;
  union foo { int ii; };
} S;

int main(){
  return S._.ui + S.ii;
}
