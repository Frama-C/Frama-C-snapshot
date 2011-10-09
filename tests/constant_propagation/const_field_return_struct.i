/* run.config
   OPT: -val -semantic-const-folding -journal-disable
*/

struct S {
  const int f0;
  int f1; } T, U;

struct S main(int c)
{
  if (c) return T;
  return U;
}
