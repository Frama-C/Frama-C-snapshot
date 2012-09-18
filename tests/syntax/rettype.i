// This test must be rejected: return type of foo is not 
// compatible between decl and def.

//@ assigns \nothing;
int foo(int* p);

//@ ensures 0 <= \result < 25;
unsigned short foo()
{
  return 0;
}
