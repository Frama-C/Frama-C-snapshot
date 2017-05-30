/* run.config
   OPT: -wp-model Typed+Ref -wp-no-print -wp-rte
*/
/* run.config_qualif
   OPT: -wp-model Typed+Ref -then -wp-rte -wp
*/
/* ************/
/* Autre solution d'une proposition de TP donné par Julien à L'IFIPS */
/* ************/

#define BOUND 100

/* La fonction [equal_elements(a, v1, v2)] prends en entrée un tableau [a] de
   longueur [BOUND+2] avec [BOUND >= 2] ne contenant que des valeurs entre [0]
   et [BOUND-1] et contenant aussi au moins deux valeurs différentes
   apparaissent deux fois (il contient donc au moins deux couples de valeurs
   égales). La fonction cherche ces deux valeurs et les stocke dans [v1] et
   [v2]. */

/*

  Cette version des spécifications utilise la notion du nombre d'occurence

*/


/*@ axiomatic Occurence {
  logic integer occ(int *a, integer low, integer high, integer v)
                    reads a[low..high-1];
  axiom base: \forall integer low, high, v; \forall int *a;
    low >= high ==> occ(a, low, high, v) == 0;
  axiom ind1: \forall integer low, high, v; \forall int *a;
    low < high ==> a[high-1] == v ==>
    occ(a, low, high, v) == 1 + occ(a, low, high-1, v);
  axiom ind2: \forall integer low, high, v; \forall int *a;
    low < high ==> a[high-1] != v ==>
    occ(a, low, high, v) == occ(a, low, high-1, v);
  } */


/*@ requires BOUND >= 2;
  @ requires \valid(v1);
  @ requires \valid(v2);
  @ requires \valid(a+(0.. BOUND+1));

  @ requires \forall integer i; 0 <= i < BOUND+2 ==> 0 <= a[i] < BOUND;
  @ requires \exists integer v1, v2;
      0 <= v1 < BOUND && 0 <= v2 < BOUND &&
      2 <= occ(a,0,BOUND+2,v1) && 2 <= occ(a,0,BOUND+2,v2) && v1 != v2;

  @ ensures v1_good: 2 <= occ(a,0,BOUND+2,*v1);
  @ ensures v2_good: 2 <= occ(a,0,BOUND+2,*v2);
  @ ensures v1_v2_diff: *v1 != *v2;
  @ assigns *v1, *v2;
  @ */
void equal_elements(int *a, int *v1, int *v2)
{
  char seen[BOUND];

  *v1 = -1;
  *v2 = -1;

  /*@ loop invariant 0 <= i <= BOUND;
    @ loop invariant set_at_0: \forall integer j; 0 <= j < i ==> 0 == seen[j];
    @
    @ loop assigns i, seen[0.. BOUND-1];
    @ loop variant BOUND-i; */
  for(int i = 0; i < BOUND; i++){
  ICI:
    seen[i] = 0;
    /*@ assert set_at_1: \forall integer j; i != j ==>
      \at(seen[j],ICI) == seen[j]; @*/
  }

  /*@ loop invariant bound: 0 <= i <= BOUND+2;

    @ loop invariant v1_first: *v1 == -1 ==> *v2 == -1;
    @ loop invariant v1_sound1: *v1 != -1 ==> 2 <= occ(a,0,i,*v1);
    @ loop invariant v2_sound1: *v2 != -1 ==> 2 <= occ(a,0,i,*v2) && *v1 != *v2;
    @ loop invariant v1_v2_diff: *v1 != -1 ==> *v1 != *v2;

    @ loop invariant seen_sound1:
    @   \forall integer v; 0 <= v < BOUND ==> seen[v]
    @   ==> 0 < occ(a,0,i,v);

    @ loop invariant seen_sound2:
    @   \forall integer v; 0 <= v < BOUND ==> ! seen[v]
    @   ==> 0 == occ(a,0,i,v);


    @ loop invariant v1_sound2:
    @   *v1 == -1 ==>
    @   \forall integer v; 0 <= v < BOUND ==> occ(a,0,i,v) < 2;

    @ loop invariant v2_sound2:
    @   *v2 == -1 ==>
    @   \forall integer v; 0 <= v < BOUND ==> v != *v1 ==>  occ(a,0,i,v) < 2;

    @ loop assigns i, seen[0.. BOUND-1], *v1, *v2;
    @ loop variant BOUND+2-i; */
  for(int i = 0; i < BOUND+2; i++) {
    int v = a[i];
    if (seen[v]) {
      if (*v1 == -1) *v1 = v;
      else if (*v2 == -1 && v != *v1) *v2 = v;
    } else {
      seen[v] = 1;
    }
  }

}
