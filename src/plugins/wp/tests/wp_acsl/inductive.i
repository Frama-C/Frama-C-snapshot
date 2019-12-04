/* run.config
   OPT:-wp-prover=why3 -wp-gen -wp-msg-key print-generated
*/
/* run.config_qualif
   DONTRUN:
*/

/*@ inductive is_gcd(integer a, integer b, integer d) {
    case gcd_zero:
      \forall integer n; is_gcd(n, 0, n);
    case gcd_succ:
      \forall integer a, b, d;
      is_gcd(b, a % b, d) ==> is_gcd(a, b, d);
    }
*/

/*@ lemma test_no_label:
  \forall integer a, b, d ;
  is_gcd(a, b, d) ==> is_gcd(b, d, a) ==> \false ;
*/

typedef struct _list { int element; struct _list* next; } list;

/*@ inductive reachable{L} (list* root, list* node) {
    case root_reachable{L}:
         \forall list* root; reachable(root,root);
    case next_reachable{L}:
         \forall list* root, *node;
         \valid(root) ==> reachable(root->next, node) ==> reachable(root,node);
  }
*/

/*@ lemma test_one_label{L1, L2}:
  \forall list *l1, *l2 ;
  reachable{L1}(l1, l2) ==> reachable{L2}(l1, l2) ==> \false ;
*/

/*@ predicate swap{L1, L2}(int *a, int *b, integer begin, integer i, integer j, integer end) =
       begin <= i < j < end &&
       \at(a[i], L1) == \at(b[j], L2) &&
       \at(a[j], L1) == \at(b[i], L2) &&
       \forall integer k; begin <= k < end && k != i && k != j ==>
       \at(a[k], L1) == \at(b[k], L2);

    predicate same_array{L1,L2}(int *a, int *b, integer begin, integer end) =
      \forall integer k; begin <= k < end ==> \at(a[k],L1) == \at(b[k],L2);

    inductive same_elements{L1, L2}(int *a, int *b, integer begin, integer end) {
      case refl{L1, L2}:
        \forall int *a, int *b, integer begin, end;
        same_array{L1,L2}(a, b, begin, end) ==>
        same_elements{L1, L2}(a, b, begin, end);
      case swap{L1, L2}: \forall int *a, int *b, integer begin, i, j, end;
        swap{L1, L2}(a, b, begin, i, j, end) ==>
        same_elements{L1, L2}(a, b, begin, end);
      case trans{L1, L2, L3}: \forall int* a, int *b, int *c, integer begin, end;
        same_elements{L1, L2}(a, b, begin, end) ==>
        same_elements{L2, L3}(b, c, begin, end) ==>
        same_elements{L1, L3}(a, c, begin, end);
    }
*/

/*@ lemma test_multilabel{L1, L2, L3}:
  \forall int *a, *b, integer b1, b2, e1, e2 ;
  same_elements{L1, L2}(a, b, b1, e1) ==>
  same_elements{L2, L3}(b, a, b2, e2) ==>
    \false ;
*/