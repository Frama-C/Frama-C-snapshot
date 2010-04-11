
/* ~~~ First of all, let's launch the tool :
       cd ~/frama-c; FRAMAC_SHARE=share  bin/viewer.opt tests/wp/wp_show.c 
 */

/* ~~~ Let's start with model 0 (no matter wp_bottom) */

/*@ 
  requires min <= max;
  ensures min <= \result <= max;

  behavior Cmin : assumes x < min; ensures \result == min;
  behavior Cmax : assumes x > max; ensures \result == max;
  behavior Cx : assumes min <= x <= max; ensures \result == x;

  complete behaviors Cmin, Cmax, Cx;
  disjoint behaviors Cmin, Cmax, Cx;
 */
int threshold (int min, int max, int x) {
  if (max < x) 
    x = max;
  if (x < min) 
    x = min;
  return x;
}

/* ~~~ M0 also works with arrays, structures and union */

int T [10];
typedef struct _Tstr { char a; int t[50]; float x; char * p; } Tstr;
Tstr S;

//@ ensures 0 <= j && j < 10 && i != j ==> \result == \old(T[j]);
int fstruct (int i, int j) {
  int j2 = (0 <= j && j < 10) ? j : 0;
  if (0 <= i && i < 10)
    T[i] = S.t[i];
  return T[j2];
}

/* ~~~ loops are handled */

//@ ensures \forall int i; 0 <= i < 10 ==> T[i] == 0;
void razT (void) {
  int i;
  //@ loop invariant \forall int k; 0<= k < i ==> T[k] == 0;
  for (i = 0; i < 10; i++)
    T[i] = 0;
}

/* ~~~ if one wants to use a light invariant, [loop assigns] should be used. */

void stat (int n) {
  int i_min = 0, i_max = 0;
  int min = S.t[0], max = S.t[0];
  int i, s = 0;
  /*@ loop invariant \forall int k; 0<= k < i ==> min <= S.t[k];
      loop assigns S;
      */
  for (i = 1; i < n; i++) {
    s += S.t[i];
    if (S.t[i] < min) { min = S.t[i]; i_min = i; }
    if (S.t[i] > max) { max = S.t[i]; i_max = i; }
  }
}

/* ~~~ calls are handled, but [assigns] specification is needed */


/* ~~~ M0 can also deal with pointer computation, as long there is no access */

/*@ behavior B0: ensures \result == &(T[i]) || \result == &(S.t[i]);
    behavior B1: ensures *\result >= T[i];
 */
int * fptr (int i) {
  int * p = (T[i] > S.t[i]) ? T : S.t;
  return p + i;
}

/* ~~~ let's now test [wp_bottom] option : */

/* ~~~ with bottom we can prove this property */

//@ behavior xpos : assumes x > 0; ensures \result == 1;
int fbot (int x) {
  if (x > 0) 
    x = 1;
  else {
    x = *(S.p);
    *(S.p) = 0;
  }
  return x;
}

/* ~~~ we can prove some properties with a simple model, and then use them
 * as hypotheses to prove some others. */

int main (void) { return 0 ; }
