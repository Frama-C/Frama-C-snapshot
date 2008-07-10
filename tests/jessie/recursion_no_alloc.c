
#define NULL 0

typedef struct _Node {
  int elem;
  struct _Node *next;
} Node;

/*@ predicate is_list{L}(Node *x) =
  @   x == 0 || \valid(x) && is_list(x->next);
  @
  @ logic boolean bool_is_list{L}(Node *x) =
  @   x == 0 || bool_is_list(x->next);
  @
  @ axiom is_list_def{L}: 
  @   \forall Node *x; is_list(x) <==> bool_is_list(x) == \true;
  @*/

/*@ logic integer list_length{L}(Node *x) = 
  @   bool_is_list(x) ? (x == NULL ? 0 : 1 + list_length(x->next)) : -1;
  @*/

/*@ requires \valid(x) && is_list(y);
  @ ensures is_list(x) && list_length(x) == list_length(y) + 1;
  @*/
void cons(Node *x, Node *y) {
  x->next = y;
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j recursion_no_alloc"
End:
*/
