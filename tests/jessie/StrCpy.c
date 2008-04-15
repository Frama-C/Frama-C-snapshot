#define CT_CAR_FIN_CHAINE '\0'

void StrCpy
    (char * const       Chaine1,
     const char * const Chaine2)
{
  char * pointeur       = Chaine1;
  const char * source   = Chaine2;
  while (*source)
    {
      *pointeur = *source;
      pointeur = pointeur + 1;
      source = source + 1;
    }
  *pointeur = CT_CAR_FIN_CHAINE;
  return;
}
