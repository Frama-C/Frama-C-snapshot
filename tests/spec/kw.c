typedef int assert;

assert behavior = 0;

/*@ logic assert foo(assert x) = x; */

/*@ requires behavior >= 0;
    assigns behavior \from behavior;
    ensures behavior >= 0;
*/
int main () {
  behavior++;
  return 0;
}

/*@ type List<A> = Nil | Cons(A,List<A>); */

/*@ inductive model{L}(List<integer> root, List<integer>logic_list) {

case nil{L}: model(Nil,Nil);
case cons{L}: \forall List<integer> l1,List<integer>ll1;
model{L}(l1,ll1) ==> model(Cons(0,l1),Cons(0,ll1));

} */

