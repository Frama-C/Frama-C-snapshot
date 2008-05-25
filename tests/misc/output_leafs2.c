void f(int* x);

int main() { 
  int x = 0; 
  f(&x);
  return x; 
}
