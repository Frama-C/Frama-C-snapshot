/* run.config
   OPT: -wp-simplify-is-cint
*/

/* run.config_qualif
   OPT: -wp-simplify-is-cint
*/

/** Tests the simplification of (forall x:int. P) into (forall
    x:integer. P) when P already constraint x to be in the range of
    the machine integer.
*/

/*@
  requires \forall int x; 0 <= x < size ==> t[x] < 0;
  requires 0 < size;
  ensures \forall int x; 0 <= x < size ==> 0 < t[x];
  @*/
void f(int *t, int size){

  /*@
    loop invariant 0 <= i <= size;
    loop invariant \forall int x; 0 <= x < i ==> 0 < t[x];
    loop invariant \forall int x; i <= x < size ==> t[x] < 0;
    loop assigns t[0..size-1], i;
    @*/
  for(int i=0; i<size; i++){
    t[i] = - t[i];
  }

}

/*@
  requires 0 < size;
  ensures \result == 1 ==>
    \exists int i; 0 <= i < size &&
      t[i] == x &&
      \forall int j; 0 <= j < i ==>
         t[j] != x;
  @*/
int g(int *t, int size, int x){

  /*@
    loop invariant 0 <= i <= size;
    loop invariant
      \forall int j; 0 <= j < i ==>
         t[j] != x;
    loop assigns i;
    @*/
  for(int i = 0; i < size; i++){

    if(t[i]==x) return 1;

  }

  return 0;

}
