/* run.config
   OPT: -wp-no-print -wp-rte -no-warn-signed-overflow
*/

/* run.config_qualif
   OPT: -then -wp-rte -no-warn-signed-overflow -wp
*/

// note: exo hors vérification de l'absence de débordements

/* ************/
/* Solution de TP donné par Julien à L'IFIPS */
/* ************/

/* Un sous-tableau [b] d'un tableau [a] est un sous-ensemble d'éléments
   contigüs de [a]. Par exemple, si a = { 0, 3, -1, 4 }, des sous-tableaux
   possibles sont {}, { 0 }, { 3, -1 }, { 0, 3, -1, 4 }.

   Un sous-tableau de [a] est dit maximal si la somme de ses éléments est au
   moins aussi grande que celle de n'importe quel autre sous-tableau de
   [a]. L'unique sous-tableau maximal de l'exemple précédent est { 3, -1, 4 }.

   Les sous-tableaux de taille 0 étant autorisés, un tableau avec uniquement des
   valeurs négatives a une somme maximale de 0.

   La fonction [max_subarray(a, len)] retourne la somme d'un sous-tableau
   maximal de [a], [len] étant la longueur de [a]. */

/*
  Questions:

   1. Donner une spécification en ACSL de cette fonction. Vous pouvez vous aider
   de la fonction logique [sum(int *a, integer low, integer high, integer len)]
   calculant la somme des éléments entre les indices [low] et [high] (inclus)
   d'un tableau [a] de longueur [len] et dont une version axiomatisée est
   fournie.

   2. Modifier le programme de manière à ajouter 3 variables ghosts [cur_low],
   [low] et [high] calculant respectivement:
   - l'indice minimum du sous-tableau duquel on est en train de calculer la 
   somme courante [cur];
   - l'indice minimum du sous-tableau maximal courant;
   - l'indice maximum du sous-tableau maximal courant;

   3. Prouver cette fonction avec WP et Alt-Ergo. Prouver la terminaison et
   l'absence d'accès mémoire indéfini, mais pas l'absence de débordements
   arithmétiques. Les options à fournir à Frama-C pour cela sont, dans cet
   ordre: -rte -no-warn-signed-overflow -then -wp
*/

/*@ axiomatic Sum {
  logic integer sum(int *a, integer low, integer high, integer len)
    reads a[low..high];

  axiom base: \forall integer low, high,len; \forall int *a;
    low > high ==> sum(a, low, high, len) == 0;

  axiom ind: \forall integer low, high,len; \forall int *a;
    0 <= low <= high < len ==>
    sum(a, low, high, len) == a[high] + sum(a, low, high-1, len);
  } */

/*@ requires len >= 0;
  @ requires \valid(a+(0..len-1));
  @ ensures \forall integer l, h;
  @  0 <= l <= h <= len ==> sum(a,l,h-1,len) <= \result;
  @ ensures \exists integer l, h;
  @  0 <= l <= h <= len && sum(a,l,h-1,len) == \result;
  @ assigns \nothing;
  @ */
int max_subarray(int *a, int len) {
  int max = 0;
  int cur = 0;
  /*@ ghost int cur_low = 0, low = 0, high = 0; */

  /*@ loop invariant 0 <= i <= len;
    @ loop invariant 0 <= low <= high <= i;
    @ loop invariant 0 <= cur_low <= i;
    @
    @ loop invariant cur <= max;
    @ loop invariant cur == sum(a,cur_low,i-1,len);
    @ loop invariant max == sum(a,low,high-1,len);
    @
    @ loop invariant \forall integer l;
    @   0 <= l <= i ==> sum(a,l,i-1,len) <= cur;
    @
    @ loop invariant \forall integer l, h;
    @   0 <= l <= h <= i <= len ==> sum(a,l,h-1,len) <= max;
    @
    @ loop assigns i, cur, max, cur_low, low, high;
    @ loop variant len - i; */
  for(int i = 0; i < len; i++) {
    cur = a[i] + cur;
    if (cur < 0) {
      cur = 0;
      /*@ ghost cur_low = i+1; */
    }
    if (cur > max) {
      max = cur;
      /*@ ghost low = cur_low; */
      /*@ ghost high = i+1; */
    }
  }
  return max;
}
