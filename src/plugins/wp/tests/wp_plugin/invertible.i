/* run.config_qualif
   DONTRUN:
 */

/*@ axiomatic A {
  @ logic integer a reads \nothing ;
  @ logic integer a1 reads \nothing ;
  @ logic integer a2 reads \nothing ;
  @ logic integer b reads \nothing ;
  @ logic integer b1 reads \nothing ;
  @ logic integer b2 reads \nothing ;
  @ logic integer c reads \nothing ;
  @ logic integer c1 reads \nothing ;
  @ logic integer c2 reads \nothing ;
  @ } */

/*@ requires Invertible_11: (( a1^a2  ^ b) == (a1^a2 ^ c)) <==> (b == c);
  @ requires Invertible_12: ((   a    ^ b) == (  a   ^ c)) <==> (b == c);
  @ requires Invertible_13: ((   a    ^ b) ==    a       ) <==> (b == 0) ;
  @ requires Invertible_14: (((a1|a2) ^ b) == (a1|a2)    ) <==> (b == 0) ;

  @ requires Invertible_21: ((a ^  b1^b2 ) == (c ^ b1^b2)) <==> (a == c);
  @ requires Invertible_22: ((a ^    b   ) == (c ^   b  )) <==> (a == c);
  @ requires Invertible_23: ((a ^    b   ) ==        b   ) <==> (a == 0);
  @ requires Invertible_24: ((a ^ (b1|b2)) ==     (b1|b2)) <==> (a == 0);

  @ requires Invertible_31: ((a ^  c1^c2 ) == (b ^ c1^c2)) <==> (a == b);
  @ requires Invertible_32: ((a ^    c   ) == (b ^   c  )) <==> (a == b);
  @ requires Invertible_33: ((a ^    c   ) ==        c   ) <==> (a == 0);
  @ requires Invertible_34: ((a ^ (c1|c2)) ==     (c1|c2)) <==> (a == 0);
*/
void main (void);
