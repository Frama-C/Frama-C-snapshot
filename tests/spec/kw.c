typedef int assert;

assert behavior = 0;

/*@ logic assert foo(assert x) = x; */

/*@ requires behavior >= 0;
    assigns behavior \from behavior;
    ensures behavior >= 0;
*/
int main () {
  //@ slevel 4;
  behavior++;
  struct custom { int reads, behaviors, label ; } writes;
  //@ assert custom: writes.reads + writes.behaviors <= \let global = writes.label; global;
  struct at { int module, function, global ; } include;
  //@ assert at: include.function + include.module  <= \let behaviors = include.global ; behaviors;
  struct loop { int requires, ensures, checks ; } assert;
  //@ assert loop: assert.ensures + assert.ensures <= \let reads = assert.checks; reads ;
  return 0;
}

/*@ type List<A> = Nil | Cons(A,List<A>); */

/*@ inductive model{L}(List<integer> root, List<integer>logic_list) {

case nil{L}: model(Nil,Nil);
case cons{L}: \forall List<integer> l1,List<integer>ll1;
model{L}(l1,ll1) ==> model(Cons(0,l1),Cons(0,ll1));

} */

/*@ axiomatic foo {
 logic integer func(integer i) reads behavior;
}
*/

volatile int assigns;

int ensures(volatile int* a) { return *a; }
int requires(volatile int*a, int v) { *a = v; return v; }

/*@ volatile assigns reads ensures writes requires; */

int slevel = 1000000;

//@ lemma bar: slevel >= 0;
