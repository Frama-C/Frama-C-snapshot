/*@ 
  axiomatic String {

  predicate Length_of_str_is(char * s,integer n) =
    \valid( s + (0..n) ) &&
    s[n] == 0 && 0 <= n &&
    \forall integer k ; 0 <= k < n ==> s[k] !=0
  ;

  logic integer Length{L}(char *s) reads s[..] ;

  axiom Length_def :
    \forall char *s;
    \forall integer n; 
       Length_of_str_is(s,n) ==> Length(s)==n ;

  }
*/

/*@
   requires \exists integer i; Length_of_str_is(s,i);
   assigns \nothing;
   ensures \exists integer i; Length_of_str_is(s,i) && \result == i;
 @*/
int strlen(const char *s) {
  const char *ss = s;
  /*@
      loop invariant BASE: \base_addr(s) == \base_addr(ss) ;
      loop invariant RANGE: s <= ss <= s+Length(s);
      loop invariant ZERO: \forall integer i; 0 <= i < (ss-s) ==> s[i] != 0;
      loop assigns ss;
      loop variant Length(s) - (ss-s) ;
   @*/
  while (*ss)
    ss++;

  /*@ assert END: Length_of_str_is(s,ss-s); */
  return ss - s;
}
