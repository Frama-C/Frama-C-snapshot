/*@
   predicate is_valid_int_range(int* p, int n) =
           (pred1:0 <= n) && pred2:\valid_range(p, 0, n-1);
*/

/*@ predicate P(int *p) = *p ==0 ; */

/* predicate Q(int p) = *(((char*)&p)+2) ==0 ; */

/*@ predicate R(int *p) = \valid(p) ; */

/*@ predicate S(int *p) =  \let z = 0 ; *p == \let x = 0 ; ((\let y = z ; x < y) ? 1 + 2 : (\let y = x ; y)) + 2 ; */


//@ axiomatic a { predicate P(integer v); }
//@ lemma l: P(1)?P(2):P(3) ;
