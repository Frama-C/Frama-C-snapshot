/* run.config
   OPT: -wp-no-print -wp-rte
*/

/* run.config_qualif
   OPT: -wp-prop=-left_unproved -then -wp-rte -wp -wp-prop=-left_unproved
*/

/* ******************************* */
/* Solution de TP donné par Julien */
/* ******************************* */

/* -------------------------------------------------------------------------- */
/*
  Ce fichier fournit une petite librairie de tables de hachage simplifiées:
  elles ne sont pas dynamiques et, en particulier, pas redimensionnables.

  Votre but est:
  1. écrire la spécification ACSL des fonctions fournies à partir de leurs
  spécifications informelles (mais pas du code...)
  2. prouver que chaque fonction satisfait la spécification écrite en 1.
  3. prouver l'absence d'erreurs à l'exécution via l'option -wp-rte.
*/
/* -------------------------------------------------------------------------- */

#include <limits.h>

/* -------------------------------------------------------------------------- */
/*
  Chaines de caractères simplifiées:
  toutes les chaînes de caractères de ce fichier seront supposées de longueurs
  STRING_LEN.

  La fonction [eq_string] est fournie avec sa spécification formelle déjà
  prouvée. Vous n'avez donc rien à faire pour cette fonction \o/.

  Astuce: le prédicat [EqString] peut éventuellement être utile...
*/
/* -------------------------------------------------------------------------- */

#define STRING_LEN 20

/*@ predicate EqString(char *s1, char *s2) =
  @   \forall integer i; 0 <= i < STRING_LEN ==> s1[i] == s2[i];
  @ */

/*@ requires \valid_read(s1 + (0 .. STRING_LEN - 1));
  @ requires \valid_read(s2 + (0 .. STRING_LEN - 1));
  @ assigns \nothing;
  @
  @ behavior eq:
  @   assumes EqString(s1, s2);
  @   ensures \result == 1;
  @
  @ behavior not_eq:
  @   assumes ! EqString(s1, s2);
  @   ensures \result == 0;
  @
  @ complete behaviors;
  @ disjoint behaviors;
  @ */
int eq_string(const char *s1, const char *s2) {
  int i;
  /*@ loop invariant 0 <= i <= STRING_LEN;
    @ loop invariant \forall integer j; 0 <= j < i ==> s1[j] == s2[j];
    @ loop assigns i;
    @ loop variant STRING_LEN - i;
    @*/
  for(i = 0; i < STRING_LEN; i++)
    if (s1[i] != s2[i]) return 0;
  return 1;
}

/* -------------------------------------------------------------------------- */
/*
  Structures de données.

  Les tables de hachage associent ici des clés sous forme de chaînes de
  caractères à des valeurs entières. Une fonction de hachage sur les
  chaînes de caractères vous est fournie.

  Les tables de hachage sont représentées par leur nombre d'éléments et un
  tableau de "buckets" de longueur HASHTBL_LEN.

  Les "buckets" sont eux-même des tableaux de couples (clé, valeur)
  (individuellement appelé "bucket") dont toutes les clés ont le même
  haché. Chaque tableau est de longueur fixe, BUCKET_LEN, mais le nombre
  d'éléments stockés dans celui-ci peut varier.
*/
/* -------------------------------------------------------------------------- */

#define BUCKET_LEN 10
#define HASHTBL_LEN 17

typedef struct {
  char *key; // clé, sous forme de chaînes de caractères (simplifiées)
  int value; // valeur associée à la clé
} Bucket;

typedef struct {
  Bucket buckets[BUCKET_LEN]; // tableau de couples (clé, valeur)
  int size;                   // nombre d'éléments stockés dans le tableau
} Buckets;

typedef struct {
  Buckets data[HASHTBL_LEN];  // tableau de buckets
  int size;               // nombre d'éléments stockés dans la table de hachage
} Hashtbl;

/* -------------------------------------------------------------------------- */
/*
  Fonction de hachage fournie: vous n'avez là non plus rien à faire \o/.

  La postcondition de la fonction [hash] ne peut pas être prouvée sans donner
  une définition à la fonction logique [Hash]. Ce travail n'est pas demandé ici
  et cette postcondition restera donc toujours non prouvée. Ce devrait être
  la seule...

  Astuce: la fonction logique [HashIndex] de l'axiomatique [Hash] n'est pas
  donnée que pour faire jolie. Vous avez aussi le droit d'ajouter vos propres
  définitions de prédicats et de fonctions...
*/
/* -------------------------------------------------------------------------- */

/*@ axiomatic Hash {
  @   logic unsigned long Hash(char *s) reads(s + (0 .. ));
  @   // [Hash] est un modèle abstrait de la fonction de hash codée en C
  @   // ci-dessous
  @
  @   logic integer HashIndex(Hashtbl *tbl, char *k) = Hash(k) % HASHTBL_LEN;
  @ } */

/*@ requires \valid_read(s + (0 .. STRING_LEN - 1));
  @ assigns \nothing;
  @ ensures left_unproved: \result == Hash(s);
  @ */
unsigned long hash(const char *s) {
  unsigned long h = 5381;
  int i;
  /*@ loop invariant 0 <= i <= STRING_LEN;
    @ loop assigns h, i;
    @ loop variant STRING_LEN - i; */
  for(i = 0; i < STRING_LEN; i++) {
    if (s[i]) break;
    h = ((h << 5) + h) + s[i];
  }
  return h;
}

/* -------------------------------------------------------------------------- */
/*
  Fonctions logiques et prédicats additionnels utiles.
*/
/* -------------------------------------------------------------------------- */

/*@
  @ predicate valid_tbl(Hashtbl *tbl) =
  @   \valid(tbl->data+(0 .. HASHTBL_LEN - 1));
  @
  @ predicate valid_buckets(Hashtbl *tbl) =
  @   \forall integer i;
  @     0 <= i < HASHTBL_LEN ==>
  @     \valid(tbl->data[i].buckets + (0 .. BUCKET_LEN - 1));
  @
  @ predicate valid_read_keys(Hashtbl *tbl) =
  @   \forall integer i, j;
  @     0 <= i < HASHTBL_LEN ==>
  @     0 <= j < tbl->data[i].size ==>
  @     \valid_read(tbl->data[i].buckets[j].key + (0 .. STRING_LEN - 1));
  @
  @ logic integer buckets_size(Hashtbl *tbl, char *k) =
  @   tbl->data[HashIndex(tbl, k)].size;
  @
  @ logic Bucket bucket(Hashtbl *tbl, char *k, integer i) =
  @   tbl->data[HashIndex(tbl, k)].buckets[i];
  @
  @ */

/* -------------------------------------------------------------------------- */
/*
  Exercice 0 (échauffement):

  La fonction [size] retourne le nombre d'éléments d'une table.
*/
/* -------------------------------------------------------------------------- */

/*@ requires \valid(tbl);
  @ assigns \nothing;
  @ ensures \result == tbl->size;
  @ */
int size(const Hashtbl *tbl) {
  return tbl->size;
}

/* -------------------------------------------------------------------------- */
/*
  Exercice 1:

  La fonction [init] initialise une table de hachage contenant 0 élément.
  En particulier, chaque buckets contient 0 élément.
*/
/* -------------------------------------------------------------------------- */

/*@ requires \valid(tbl);
  @ requires valid_tbl(tbl);
  @ assigns tbl->size, tbl->data[0 .. HASHTBL_LEN - 1];
  @ ensures tbl->size == 0;
  @ ensures \forall integer i; 0 <= i < HASHTBL_LEN ==> tbl->data[i].size == 0;
  @ */
void init(Hashtbl *tbl){
  int i;
  tbl->size = 0;
  /*@ loop invariant 0 <= i <= HASHTBL_LEN;
    @ loop invariant \forall integer k; 0 <= k < i ==> tbl->data[k].size == 0;
    @ loop assigns i, tbl->data[0 .. HASHTBL_LEN - 1].size;
    @ loop variant HASHTBL_LEN - i;
    @ */
  for(i = 0; i < HASHTBL_LEN; i++)
    tbl->data[i].size = 0;
}

/* -------------------------------------------------------------------------- */
/*
  Exercice 2:

  La fonction [add] ajouter un couple (clé, valeur) dans la table de hachage
  s'il y a suffisamment de places. Si tel est le cas, elle retourne 0.

  S'il n'y a pas suffisamment de places, la fonction ne fait rien et retourne
  -1.

  Conseil: lorsque la table est modifiée, bien penser à spécifier les nouvelles
  tailles et l'emplacement du couple ajouté.
*/
/* -------------------------------------------------------------------------- */

/*@ requires \valid(tbl);
  @ requires valid_tbl(tbl);
  @ requires valid_buckets(tbl);
  @ requires \valid_read(k + (0 .. STRING_LEN - 1));
  @ requires 0 <= tbl->size < INT_MAX;
  @ requires \forall integer i;
  @   0 <= i < HASHTBL_LEN ==>
  @   0 <= tbl->data[i].size <= BUCKET_LEN;
  @
  @ assigns tbl->data[HashIndex(tbl, k)], tbl->size;
  @
  @ behavior nominal:
  @   assumes buckets_size(tbl, k) < BUCKET_LEN;
  @   assigns tbl->data[HashIndex(tbl, k)], tbl->size;
  @   ensures \result == 0;
  @   ensures tbl->size == \old(tbl->size) + 1;
  @   ensures buckets_size(tbl, k) == buckets_size{Old}(tbl, k) + 1;
  @   ensures bucket(tbl, k, buckets_size{Old}(tbl, k)).key == k;
  @   ensures bucket(tbl, k, buckets_size{Old}(tbl, k)).value == d;
  @ behavior full:
  @   assumes buckets_size(tbl, k) == BUCKET_LEN;
  @   assigns \nothing;
  @   ensures \result == -1;
  @
  @ complete behaviors;
  @ disjoint behaviors;
  @ */
int add(Hashtbl *tbl, char *k, int d) {
  Bucket new_entry;
  unsigned int h = hash(k) % HASHTBL_LEN;
  if (tbl->data[h].size >= BUCKET_LEN)
    return -1;
  new_entry.key = k;
  new_entry.value = d;
  tbl->data[h].buckets[tbl->data[h].size] = new_entry;
  tbl->data[h].size++;
  tbl->size++;
  return 0;
}

/* -------------------------------------------------------------------------- */
/*
  Exercice 3:

  La fonction [mem_binding] retourne 1 si le couple (clé, valeur) [k, v] donné
  en entrée est présent dans la table de hachage. Elle retourne 0 sinon.
*/
/* -------------------------------------------------------------------------- */

/*@ requires \valid(tbl);
  @ requires valid_tbl(tbl);
  @ requires valid_buckets(tbl);
  @ requires valid_read_keys(tbl);
  @ requires \valid_read(k + (0 .. STRING_LEN - 1));
  @ requires \forall integer i;
  @   0 <= i < HASHTBL_LEN ==>
  @   0 <= tbl->data[i].size < BUCKET_LEN;
  @
  @ assigns \nothing;
  @
  @ behavior found:
  @   assumes \exists integer i; 0 <= i < buckets_size(tbl, k) &&
  @     EqString(k, bucket(tbl, k, i).key) && v == bucket(tbl, k, i).value;
  @   ensures \result == 1;
  @
  @ behavior not_found:
  @   assumes \forall integer i; 0 <= i < buckets_size(tbl, k) ==>
  @     (! EqString(k, bucket(tbl, k, i).key)
  @     || tbl->data[HashIndex(tbl, k)].buckets[i].value != v);
  @     // note: Alt-Ergo does not manage to prove it if written
  @     // bucket(tbl, k, i).value != v
  @     // so disappointing :-(
  @   ensures \result == 0;
  @
  @ complete behaviors;
  @ disjoint behaviors;
  @ */
int mem_binding(const Hashtbl *tbl, const char *k, int v) {
  int i, h = hash(k) % HASHTBL_LEN;
  /*@ loop invariant 0 <= i <= tbl->data[h].size;
    @ loop invariant \forall integer j; 0 <= j < i ==>
    @  (! EqString(k, tbl->data[h].buckets[j].key)
    @  || tbl->data[h].buckets[j].value != v);
    @ loop assigns i;
    @ loop variant tbl->data[h].size - i;
    @ */
  for(i = 0; i < tbl->data[h].size; i++) {
    if (eq_string(k, tbl->data[h].buckets[i].key)
        && tbl->data[h].buckets[i].value == v)
      return 1;
  }
  return 0;
}
