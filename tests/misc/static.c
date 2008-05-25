int * f (void)
{
  static int x;
  return &x;
}

int main() {
  *(f()) = 3;
  return *f();
}
  
