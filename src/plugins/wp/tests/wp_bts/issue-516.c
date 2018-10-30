/* run.config_qualif
   DONTRUN:
*/

#define NULL ((void*) 0)

struct list {
  struct list* next ;
  int field ;
};

/*@ axiomatic To_ll {
  @   logic \list<struct list*> to_logic_list{L}(struct list* bl, struct list* el) ;
  @ }*/

typedef struct list **list_t ;

void list_remove(list_t list, struct list *item){
  struct list *l = *list ;
  int n = 0 ;
  /*@ assert  \nth(to_logic_list(*list, NULL), n) == l && l != NULL;*/
  /*@ assert UNROLL:
    (l->next != item && l->next == NULL) ==>
    to_logic_list(*list, NULL) == (to_logic_list(*list, l) ^ to_logic_list(l, NULL));
  */
}
