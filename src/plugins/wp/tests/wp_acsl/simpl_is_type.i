/* run.config
   OPT: -wp-simplify-is-cint
*/

/* run.config_qualif
   OPT: -wp-simplify-is-cint -wp-prop=-ko,-lack
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

//@ axiomatic A { predicate P(integer x, integer y, real f) reads \nothing; }
void check_acsl (void) {
  //@ check ko: A1: absorb_is_cint: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 0 <= x < 64 ==> P(x,y,f);
  //@ check ko: A2: absorb_is_cint: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 0 <= x < 64 && P(y,x,1.0) ==> P(x,y,f);
  //@ check ko: A3: absorb_is_cint: \exists integer y ; \forall unsigned char x ; \exists real f ; 0 <= x < 64 ==> P(x,y,f);
  //@ check ko: A4: absorb_is_cint: \exists integer y ; \forall unsigned char x ; \exists real f ; 0 <= x < 64 && P(y,x,f) ==> P(x,y,f);

  //@ check ok:   C1: absurd_is_cint: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; -900000 <= x < 0 && P(y,x,f) ==> P(x,y,f);
  //@ check ok:   C2: absurd_is_cint: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; -900000 <= x < 0 && P(y,x,f) ==> P(x,y,f);
  //@ check lack: C3: absurd_is_cint: \exists integer y ; \forall unsigned char x ; \exists real f ; -900000 <= x < 0 && P(y,x,f) ==> P(x,y,f);
  //@ check lack: C4: absurd_is_cint: \exists integer y ; \forall unsigned char x ; \exists real f ; -900000 <= x < 0 && P(y,x,f) ==> P(x,y,f);
  //@ check ok:   C5:     absurd_cmp: \let f = 1.0; \forall integer x ; (\exists integer y ; 0 < y < 1) ==> P(3,5,f);
  //@ check ko:   B5:  no_absurd_cmp: \let f = 1.0; \forall integer x ; (\exists real    y ; 0 < y < 1) ==> P(3,5,f);

  //@ check ko: Min1: reduces_min: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; -5 <= x < 100 ==> P(x,y,f);
  //@ check ko: Min2: reduces_min: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 10 <= x < 100 && P(10,y,f) ==> P(x,y,f);
  //@ check ko: Min3: reduces_min: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 10 <= x < 100 && P(10,y,f) && P(11,y,f) && P(13,y,f) ==> P(x,y,f);
  //@ check ko: Min4: reduces_min: \exists integer y ; \forall unsigned char x ; \exists real f ; -5 <= x < 100 ==> P(x,y,f);
  //@ check ko: Min5: reduces_min: \exists integer y ; \forall unsigned char x ; \exists real f ; 10 <= x < 100 && P(10,y,f) ==> P(x,y,f);
  //@ check ko: Min6: reduces_min: \exists integer y ; \forall unsigned char x ; \exists real f ; 10 <= x < 100 && P(10,y,f) && P(11,y,f) && P(13,y,f) ==> P(x,y,f);

  //@ check ko: Max1: reduces_max: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 10 <= x < 600 ==> P(x,y,f);
  //@ check ko: Max2: reduces_max: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 10 <= x < 100 && P(98,y,f) && P(99,y,f) ==> P(x,y,f);
  //@ check ko: Max3: reduces_max: \exists integer y ; \forall unsigned char x ; \exists real f ; 10 <= x < 600 ==> P(x,y,f);
  //@ check ko: Max4: reduces_max: \exists integer y ; \forall unsigned char x ; \exists real f ; 10 <= x < 100 && P(98,y,f) && P(99,y,f) ==> P(x,y,f);

  //@ check ko: MinMax1: reduce_minmax: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; -5 <= x < 100 && P(98,y,f) && P(99,y,f) ==> P(x,y,f);
  //@ check ko: MinMax2: reduce_minmax: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 10 <= x < 100 && P(10,y,f) && P(11,y,f) && P(13,y,f) &&P(98,y,f) && P(99,y,f) ==> P(x,y,f);
  //@ check ko: MinMax3: reduce_minmax: \exists integer y ; \forall unsigned char x ; \exists real f ; -5 <= x < 100 && P(98,y,f) && P(99,y,f) ==> P(x,y,f);
  //@ check ko: MinMax4: reduce_minmax: \exists integer y ; \forall unsigned char x ; \exists real f ; 10 <= x < 100 && P(10,y,f) && P(11,y,f) && P(13,y,f) &&P(98,y,f) && P(99,y,f) ==> P(x,y,f);


  //@ check ko: Let1: intro_let: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 10 <= x < 11 ==> P(x,y,f);
  //@ check ko: Let2: intro_let: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; -5 <= x < 1 ==> P(x,y,f);
  //@ check ko: Let3: intro_let: \exists integer y ; \forall unsigned char x ; \let f = 1.0   ; 255 <= x < 600 ==> P(x,y,f);
  //@ check ko: Let4: intro_let: \exists integer y ; \forall unsigned char x ; \exists real f ; 10 <= x < 11 ==> P(x,y,f);
  //@ check ko: Let5: intro_let: \exists integer y ; \forall unsigned char x ; \exists real f ; -5 <= x < 1 ==> P(x,y,f);
  //@ check ko: Let6: intro_let: \exists integer y ; \forall unsigned char x ; \exists real f ; 255 <= x < 600 ==> P(x,y,f);

}
