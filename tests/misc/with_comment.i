/* run.config
  GCC:
  OPT: -val -deps -out -input  -main main2 -journal-disable
*/
/* Commentaire avant G comment*/ /* Commentaire avant G2 comment*/
static int G;
/* Commentaire apres G avant main comment*/


int main2 () {
/* Commentaire apres main comment*/
  int i;
/* Commentaire apres int i comment
   Big Comment line 1
   Bif Comment line 2 */
  G = 0;

/* Commentaire avant loop comment*/
 /*@ loop pragma UNROLL 0; */
  for(i=0; i<=10; i++)
    G++;

// AVANT j
  {int /* milieu jcomment*/ j;
  j = /* milieu j 2comment*/ 0; }
// APRES j

  return i;
}

/* ICI avant H comment*/
static int H;
/* ICI apres H comment*/
// fin

int HHH;
