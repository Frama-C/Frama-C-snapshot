/* run.config
   OPT: -wp-no-print -wp-rte
*/

/* run.config_qualif
   OPT: -then -wp-rte -wp
*/

/* ************/
/* Solution de TP donné par Julien à L'IFIPS */
/* ************/

#define BOUND 100

/* La fonction [pair(a, len)] prend en entrée un tableau [a] de longueur
   [len] ne contenant que des valeurs entre 0 et [BOUND-1] (inclus) et retourne
   [-1] si [a] ne contient pas deux éléments de même valeur et retourne un des
   indices correspondant à une valeur en double sinon. */

/*
  Questions:

  1. Définir un prédicat [has_pair(a, len)] qui est valide si et seulement s'il
  existe (au moins) deux éléments du tableau [a] de longueur [len] qui sont
  égaux.

  2. En utilisant des comportements (behaviors), donner une spécification en
  ACSL de la fonction [pair].

  3. Prouver cette fonction en utilisant le WP et Alt-Ergo. La preuve doit
  inclure la terminaison et l'absence d'erreur à l'exécution.
*/

/*@ predicate has_pair(int *a, integer len) =
  \exists integer i, j; 0 <= i < j < len && a[i] == a[j]; */

/*@ requires len >= 0;
  @ requires \valid(a+(0.. len-1));
  @ requires \forall integer i; 0 <= i < len ==> 0 <= a[i] < BOUND;
  @ assigns \nothing;
  @
  @ behavior no_pair:
  @   assumes ! has_pair(a, len);
  @   ensures \result == -1;
  @
  @ behavior has_pair:
  @   assumes has_pair(a, len);
  @   ensures \exists integer i; 
  @     0 <= i < len && i != \result && a[\result] == a[i];
  @
  @ complete behaviors;
  @ disjoint behaviors;
  @ */
int pair(int *a, int len)
{
  char seen[BOUND];

  /*@ loop invariant 0 <= i <= BOUND;
    @ loop invariant \forall integer j; 0 <= j < i ==> seen[j] == 0;
    @
    @ loop assigns i, seen[0.. BOUND-1];
    @ loop variant BOUND-i; */
  for(int i = 0; i < BOUND; i++)
    seen[i] = 0;

  /*@ loop invariant 0 <= i <= len;
    @
    @ loop invariant
    @   \forall integer v; 0 <= v < BOUND ==> seen[v]
    @   ==> \exists integer j; 0 <= j < i && a[j] == v;
    @
    @ loop invariant
    @   \forall integer v; 0 <= v < BOUND ==> ! seen[v]
    @   ==> \forall integer j; 0 <= j < i ==> a[j] != v;
    @
    @ loop invariant !has_pair(a, i);
    @
    @ loop assigns i, seen[0.. BOUND-1];
    @ loop variant len-i; */
  for(int i = 0; i < len; i++) {
    int v = a[i];
    if (seen[v]) return i;
    else seen[v] = 1;
  }

  return -1;
}
