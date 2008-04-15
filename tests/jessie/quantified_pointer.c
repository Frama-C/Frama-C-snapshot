
#ifndef NULL
#define NULL 0
#endif /* NULL */

typedef struct _Node {
  int elem;
  struct _Node *next;
} Node;

/*@ inductive reachable{L}(Node *x, Node *y) {
  @   case empty{L}: \forall Node *x; reachable(x,x);
  @   case not_empty{L}: 
  @      \forall Node *x,*y; 
  @           \valid(x) && reachable(x->next,y) ==> reachable(x,y);
  @ }
  @*/

/*@ predicate is_list{L}(Node *x) = reachable(x,(Node*)NULL);
  @  
  @ predicate is_in_list{L}(Node *x, integer elem) = 
  @   \exists Node *y; reachable(x,y) && y->elem == elem;
  @
  @ predicate is_cycle{L}(Node *x) = 
  @   \exists Node *y; reachable(x,y) && y->next == x;
  @*/

/*@ requires is_in_list(x,0);
  @ assigns \nothing;
  @*/
void f(Node *x);

/*
Local Variables:
compile-command: "LC_ALL=C make -j quantified_pointer.sep"
End:
*/
