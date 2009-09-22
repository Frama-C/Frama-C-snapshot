typedef

struct _list
  { int element;
    struct _list* next; } list;

/*@ type List<A> = Nil | Cons(A,List<A>); */

/*@ inductive model{L}(list* root, List<int>logic_list) {

case nil{L}: model(\null,Nil);
case cons{L}: \forall list* l1,List<int>ll1; \valid(l1) ==>
model(l1->next,ll1) ==> model(l1,Cons(l1->element,ll1));

} */
