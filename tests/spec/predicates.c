/*@
   predicate is_valid_int_range(int* p, int n) =
           (0 <= n) && \valid_range(p, 0, n-1);
*/

/*@ predicate P(int *p) = *p ==0 ; */

/* predicate Q(int p) = *(((char*)&p)+2) ==0 ; */

/*@ predicate R(int *p) = \valid(p) ; */


