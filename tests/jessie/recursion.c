
#define NULL 0

typedef struct _List {
  int elem;
  struct _List *next;
} List;

/*@ predicate is_list{L}(List *x) =
  @   x == 0 || \valid(x) && is_list(x->next);
  @*/

/*@ logic integer list_length{L}(List *x) = 
  @   x == NULL ? 0 : 1 + list_length(x->next);
  @*/

/* axiom list_def{L}: 
  @   \forall List *x; is_list(x) ==> list_length(x) >= 0;
  @*/

/*@ requires is_list(x);
  @ ensures is_list(\result) && list_length(\result) == list_length(x) + 1;
  @*/
List* cons(int elem, List *x) {
  List *hd = (List*) malloc(sizeof(List));
  hd->elem = elem;
  hd->next = x;
  return hd;
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j recursion"
End:
*/
