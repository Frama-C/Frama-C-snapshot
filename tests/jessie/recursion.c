
#ifndef NULL
#define NULL 0
#endif /* NULL */

typedef struct _List {
  int elem;
  struct _List *next;
} List;

/*@ axiomatic IsList {
  @ predicate is_list{L}(List *x);
  @ logic integer list_length{L}(List *x);
  @ }
  @ */

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
