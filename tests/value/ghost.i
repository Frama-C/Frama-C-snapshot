
/* Commentaire avant G */ /* Commentaire avant G2 */
int G;
/* Commentaire apres G avant main */

/*@ ghost int GHOST ; */

int main () {
/* Commentaire apres main */
  int i;
/* Commentaire apres int i */
  G = 0;
/*@ghost GHOST=G+G ; */
/* Commentaire avant loop */
 /*@ loop pragma UNROLL 0; */
  for(i=0; i<=10; i++)
    G++;

// AVANT j
  {int /* milieu j*/ j;
  j = /* milieu j 2*/ 0; }
// APRES j

  return i;
}

/* ICI avant H */
int H;
/* ICI après H */
