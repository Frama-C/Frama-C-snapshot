struct T {
  int ok;
  int* pt;
  int tab[10];
} u ;

//@ type invariant pt_validity(struct T t) = t.ok ==> \valid(t.pt) ;

/*@ type invariant tab_nonnegative(struct T t) =
  @    \forall int i; 0 <= i && i < 10 ==> t.tab[i] >=0 ;
  @*/

//@ type invariant strange(struct T t) = t == u ;

//@ global invariant u_inv: u.ok == 1;

// error: redefined invariant.
//@ global invariant u_inv: u.ok <= 1;
