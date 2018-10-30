int a = -12;
int b;
unsigned int *p=&a;
unsigned int *q=&b;
int X, Y, Z, T;

int main(int c){
  b = c ? -1 : 5;
  if (*p == 3) 
    X = *p;
  else
    Y = *p;
  if (*q == ((unsigned int)-1)) // Evaluating this condition changes the value of b to a set of unsigned values. This is not ideal, but not really problematic either, as we always recast binary representations to the good type.
    Z = *q;
  else
    T = *q;
  Frama_C_show_each(b);
  Frama_C_dump_each();
  b = b+0;
  return 0;
}
