/*@
  type list<A> = Nil | Cons(A,list<A>);

  axiomatic length {
  logic integer length<B> (list<B> l);
  axiom length_empty<C>: length(Nil) == 0;
  axiom length_cons<D>: \forall D a, list<D> l;
  length(Cons(a,l)) == length(l)+1;
  }
 */

/*@

  type my_list = list<integer>;
  logic my_list foo = Cons(1,Nil);

*/

/*@

  type other_list<B> = list<B>;
  logic other_list<int> bar = Cons((int)42, Nil);

 */

/*@
  lemma foo: length(bar) == length(foo);
 */

/*@ type my_int = int;

  logic my_int x = (int) 42;

  lemma baz: x + 1== 43;

 */
