/* run.config
   OPT: -wp-no-print -wp-rte
*/

/* run.config_qualif
   OPT: -then -wp-rte -wp
*/

/* ************/
/* Solution de TP donné par Julien à L'IFIPS */
/* ************/

/*
  Questions:

   1. Informellement, que calcule cette fonction?

   2. Donner une spécification en ACSL de cette fonction.

   3. Prouver cette fonction avec WP et Alt-Ergo. La preuve doit inclure la
  terminaison et l'absence d'erreur à l'exécution.

*/

/*@ requires len > 0;
  @ requires \valid(a+(0..len-1));
  @ ensures 0 <= \result < len;
  @ ensures \forall integer i; 0 <= i < len ==> a[i] <= a[\result];
  @ assigns \nothing; */
int exo1(int *a, int len) {
  int i = 0, j = len - 1;
  /*@ loop invariant 0 <= i <= j < len;
    @ loop invariant
    @  \forall integer k;
    @         (0 <= k < i || j < k < len)
    @     ==> (a[k] <= a[i] || a[k] <= a[j]);
    @ loop assigns i, j;
    @ loop variant j - i; */
  while (i < j)
    if (a[i] <= a[j]) i++;
    else j--;
  return i;
}
