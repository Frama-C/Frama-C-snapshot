//@ ghost int WORLD;

/*@
  axiomatic Event {
  type evt = WrOf(integer) | RdOf(integer) ;
  logic \list<evt> obs{L} reads WORLD ;

  
  lemma cons: \forall integer i,j; (WrOf(i) == WrOf(j)) <==> (i == j);
  lemma diff: \forall integer i,j; (RdOf(i) != WrOf(j));

  }
*/
