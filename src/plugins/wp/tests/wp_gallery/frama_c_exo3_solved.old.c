/* run.config
   OPT: -wp-model Typed+Ref -wp-no-print -wp-rte
*/
/* run.config_qualif
   OPT: -wp-model Typed+Ref -then -wp-rte -wp
*/

/* ************/
/* Solution d'une proposition TP donné par Julien à L'IFIPS */
/* ************/

#define BOUND 100

/* La fonction [equal_elements(a, v1, v2)] prends en entrée un tableau [a] de
   longueur [BOUND+2] avec [BOUND >= 2] ne contenant que des valeurs entre [0]
   et [BOUND-1] et contenant aussi au moins deux valeurs différentes
   apparaissent deux fois (il contient donc au moins deux couples de valeurs
   égales). La fonction cherche ces deux valeurs et les stocke dans [v1] et
   [v2]. */

/*
  Questions:

  1. Remplacer le corps du prédicat [appear_twice(int *a, integer len, integer
  v)] qui prend un tableau [a] de longueur [len] et une valeur [v] de façon à ce
  qu'il soit valide si et seulement si [v] apparaît au moins deux fois dans [a].

  Afin de faciliter la tâche ultérieure d'Alt-Ergo (question 3), on veillera à
  faire en sorte de limiter au maximum la portée des quantificateurs. Ainsi, on
  préférera par exemple écrire
     
    \forall integer i; i == 0 ==> \exists integer j; i == j;

  plutôt que
     
    \forall integer i; \exists integer j; i == 0 ==> i == j;


  2. Donner une spécification en ACSL de la fonction [equal_elements].

  3. Prouver cette fonction en utilisant le WP avec le modèle Typed+ref et
  Alt-Ergo. La preuve doit inclure la terminaison et l'absence d'erreur à
  l'exécution.

  Pour activer le modèle Typed+ref directement à partir de la ligne de commandes
  (plutôt que dans la GUI), vous pouvez fournir l'option -wp-model Typed+ref à
  Frama-C.

  Les deux assertions dans le code doivent également être prouvées. Elles
  facilitent la tâche d'Alt-Ergo (et peuvent vous permettre de mieux comprendre
  l'algorithme).

*/

/*@ predicate appear_twice(int *a, integer len, integer v) =
  \exists integer i; 0 <= i < len && a[i] == v &&
  \exists integer j; 0 <= j < len && a[j] == v && i != j; */

/*@ requires BOUND >= 2;
  @ requires \valid(v1);
  @ requires \valid(v2);
  @ requires \valid(a+(0.. BOUND+1));
  
  @ requires \forall integer i; 0 <= i < BOUND+2 ==> 0 <= a[i] < BOUND;
  @ requires \exists integer v1, v2;
  @   appear_twice(a, BOUND+2, v1) && appear_twice(a, BOUND+2, v2) && v1 != v2;

  @ ensures appear_twice(a, BOUND+2, *v1);
  @ ensures appear_twice(a, BOUND+2, *v2);
  @ ensures *v1 != *v2;
  @ assigns *v1, *v2;
  @ */
void equal_elements(int *a, int *v1, int *v2)
{
  char seen[BOUND];

  *v1 = -1;
  *v2 = -1;

  /*@ loop invariant 0 <= i <= BOUND;
    @ loop invariant \forall integer j; 0 <= j < i ==> seen[j] == 0;
    @
    @ loop assigns i, seen[0.. BOUND-1];
    @ loop variant BOUND-i; */
  for(int i = 0; i < BOUND; i++)
    seen[i] = 0;

  /*@ loop invariant 0 <= i <= BOUND+2;

    @ loop invariant *v1 == -1 ==> *v2 == -1;
    @ loop invariant *v1 != -1 ==> appear_twice(a, i, *v1);
    @ loop invariant *v2 != -1 ==> appear_twice(a, i, *v2) && *v1 != *v2;
    @ loop invariant *v1 != -1 ==> *v1 != *v2;

    @ loop invariant
    @   \forall integer v; 0 <= v < BOUND ==> seen[v]
    @   ==> \exists integer j; 0 <= j < i && a[j] == v;

    @ loop invariant
    @   \forall integer v; 0 <= v < BOUND ==> ! seen[v]
    @   ==> \forall integer j; 0 <= j < i ==> a[j] != v;

    @ loop invariant
    @   *v2 == -1 ==>
    @   \forall integer v; 0 <= v < BOUND ==>
    @   v != *v1 ==> ! appear_twice(a, i, v);

    @ loop assigns i, seen[0.. BOUND-1], *v1, *v2;
    @ loop variant BOUND+2-i; */
  for(int i = 0; i < BOUND+2; i++) {
    int v = a[i];
    if (seen[v]) {
      if (*v1 == -1) *v1 = v;
      else if (*v2 == -1 && v != *v1) *v2 = v;
      /*@ assert
        @   *v2 == -1 ==>
        @   \forall integer w; ! appear_twice(a, i, w) ==> w != *v1 
        @     ==> ! appear_twice(a, i+1, w); */
    } else {
      seen[v] = 1;
      /*@ assert
        @   *v2 == -1 ==>
        @   \forall integer w; ! appear_twice(a, i, w) ==> w != *v1 
        @     ==> ! appear_twice(a, i+1, w); */
    }
  }
}
