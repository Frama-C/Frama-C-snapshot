struct S { int b:31; } s;

int f(void)
{
  return -1;
}

int main(){
  s.b = f();
  Frama_C_dump_each();
  return 0;
}
