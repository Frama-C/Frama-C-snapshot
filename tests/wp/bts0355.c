int * P;
int X;
/*@ requires \separated(P, &X);
    behavior default: ensures (*P > X); */
void f_with_hyp(void)
{
  int x ;
  x = X;
  *P = x + 1;
  return;
}

int main (void) {return 0;}
