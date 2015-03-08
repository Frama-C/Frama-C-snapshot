/* run.config_qualif                                                                  
   DONTRUN: [PB] temporary removed since a difference has to be validated.
*/

/* run.config_qualif
   OPT: -wp-par 1 -load-script tests/wp_plugin/combined.ml
*/

/* ZD : this should not be here such as it cannot be tested by all frama-c
        developer
 */

/*@ axiomatic A {
  @ predicate P(int x);
  @ }*/

/*@ ensures P(\result);
  @ assigns \nothing; */
int f(int i);

/*@ assigns \nothing; */
int g(int j);

void job(int *t, int A) {

  /*@ assert 50 <= A <= 100; */

  /*@ loop invariant 0 <= i <= 50;
    @ loop invariant \forall integer k; 0 <= k < i ==> P(t[k]);
    @ loop assigns i,t[0..49];
    @ */
  for(int i = 0; i < 50; i++) t[i] = f(i);

  /*@ loop invariant A <= j <= 100;
    @ loop assigns j,t[A..99];
    @ */
  for(int j = A; j < 100; j++) t[j] = g(j);

  /*@ assert \forall integer k; 0 <= k < 50 ==> P(t[k]); */

}

int T[100];

void main(void) {
  job(T, 50);
  //  job(T, 48);
}
