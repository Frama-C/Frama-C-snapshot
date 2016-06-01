struct S { int b:31; } s;

int f(void)
{
  return -1;
}

main(){
  s.b = f();
  Frama_C_dump_each();
}
  
