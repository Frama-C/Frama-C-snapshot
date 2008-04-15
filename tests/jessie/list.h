

/**************************************
 * NULL-terminated linked-lists of ints
 **************************************/

#ifndef NULL
#define NULL 0
#endif

typedef struct struct_list {
  int hd;
  struct struct_list * tl;
} *linked_list;


/***************************************
 * logic lists of llist pointers
 ***************************************/

// should be polymorphic, and defined before linked_list structure

/*@ axiomatic List {
  @
  @   type list;
  @
  @   logic list nil ;
  @
  @   logic list cons(linked_list l, list l) ;
  @
  @   logic list app(list l1, list l2);
  @
  @   axiom app_nil_right: \forall list l; l == app(l,nil) ;
  @
  @   axiom app_nil_left: \forall list l; l == app(nil,l) ;
  @
  @   logic list rev(list l) ;
  @
  @   axiom rev_nil: rev(nil) == nil;
  @
  @   predicate disjoint(list l1, list l2);
  @
  @   axiom disjoint_nil_right: \forall list l; disjoint(l,nil);
  @
  @   axiom disjoint_nil_left: \forall list l; disjoint(nil,l);
  @
  @   logic int len(list l);
  @
  @   axiom len_nil: len(nil) == 0;
  @
  @   axiom len_cons:
  @     \forall linked_list p, list l; len(cons(p,l)) == 1+len(l);
  @
  @   lemma len_app:
  @     \forall list l1,l2; len(app(l1,l2)) == len(l1)+len(l2);
  @
  @ }
  @*/

/************************************
 * paths and NULL-terminated lists
 **********************************/

/*@ inductive linked_path{L}(linked_list p1, list l, linked_list p2) {
  @   case path_null{L}: \forall linked_list p; linked_path(p,nil,p);
  @   case path_cons{L}:
  @     \forall linked_list p1,p2; \forall list l;
  @      \valid(p1) && linked_path(p1->tl,l,p2)
  @         ==> linked_path(p1,cons(p1,l),p2) ;
  @ }
  @
  @ // WRONG !!!
  @ // lemma linked_path_null_inv :
  @ //  \forall linked_list p; \forall list l; lpath(p,l,p) ==> l==nil;
  @
  @ lemma linked_path_cons_inv{L} :
  @   \forall linked_list p1,p2,p3 ; \forall list l;
  @     linked_path(p1,cons(p2,l),p3) ==>
  @        p1 == p2 && \valid(p1) && linked_path(p1->tl,l,p3) ;
  @ 
  @ predicate list_contents{L}(linked_list p, list l) = linked_path(p,l,\null);
  @
  @ lemma list_contents_functional{L} :
  @   \forall linked_list p; \forall list l1,l2;
  @      list_contents(p,l1) && list_contents(p,l2) ==> l1==l2;
  @
  @ lemma list_contents_valid{L} :
  @   \forall linked_list p; \forall list l;
  @      list_contents(p,l) && p != \null ==> \valid(p);
  @
  @ predicate null_term_list{L}(linked_list p) =
  @   \exists list l ; list_contents(p,l);
  @
  @*/

/* TODO ?
  @ predicate finite(list l) reads l->tl
  @
  @ predicate cyclic(list l) reads l->tl
  @
  @ type Length
  @ logic Length length(list l) reads l->tl
  @ predicate length_order(Length l1, Length l2)
*/
