int tab[16];

void* main(void)
{
  int i;

  static const int* t[] = {
    &tab[1],
    &tab[3],
    &tab[4],
    &i
  };

  return &t;
}
