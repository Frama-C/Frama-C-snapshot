/* run.config
   COMMENT: alias
*/

void f(int* dest, int val)
{
  int *ptr = dest;
  *ptr = val;
}

int main() {
  int i;
  f(&i, 255);
  /*@ assert \initialized(&i); */
  return 0;
}
