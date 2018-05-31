/*@ logic integer size_rec(int* busybits, integer capa) =
  @ (capa <= 0) ? 0 :
  @ (busybits[capa-1] != 0) ? 1 + size_rec(busybits, capa - 1) :
  @ size_rec(busybits, capa - 1);
*/

/*@ ensures \result == size_rec(busybits, 0); */
int size(int *busybits) {
    return 0;
}

/*@ logic integer f(int* p, integer l) = (l > 0)? 1 + f(p+1,l-1) : *p; */

/*@ predicate p(int *p, integer l) = (l > 0) ? p(p+1, l-1) : \valid(p); */

/*@ inductive foo(int* p, integer l) {
  case nil: \forall int* p; foo(p,0);
  case other: \forall int* p, integer l;
    l>0 ==> \valid(p+l) ==> foo(p,l-1) ==> foo(p,l);
  }
*/
