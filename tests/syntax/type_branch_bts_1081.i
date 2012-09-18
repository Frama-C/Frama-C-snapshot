main(){
  int foo, x, y;
  foo ? (void)x : (signed char)y; 
  // accepted (we drop the expressions, don't care about their types)
  int z = foo ? (void)x: (signed char)y; // rejected
  return 0;
}
