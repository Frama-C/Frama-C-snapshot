/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main g -journal-disable
*/
int Sa;
int Sb;


void f (void)
{
  int c = 12;

  if (c)
    P (Sa);

  P (Sa);
  P (Sb);

  V (Sa);
  V (Sb);
}

void g (void)
{
  int c = -25;


  while (c--)
    while (c)
      {
        V (Sa);
        c++;
        }
  P (Sb);
  P (Sa);

  V (Sa);
  V (Sb);

  f();
}

/*
void creation_tache( void (*f)(void)) {
  (*f)();
};
void main (void)
{
  Screation_tache (&f);
  Screation_tache (&g);
}
*/
