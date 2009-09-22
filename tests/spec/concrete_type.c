/*@
  type list<A> = Nil | Cons(A,list<A>);

  axiomatic length {
  logic integer length<A> (list<A> l);
  axiom length_empty<A>: length(Nil) == 0;
  axiom length_cons<A>: \forall A a, list<A> l;
  length(Cons(a,l)) == length(l)+1;
  }
 */
