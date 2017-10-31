/* run.config
   OPT: -wp-prover coq -wp-gen -wp-msg-key print-generated
*/

/* run.config_qualif
   OPT: -wp-prover coq -wp-script tests/wp_plugin/inductive.script
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

/*@ lemma test: 
    \forall list *root,*node; 
         reachable(root,node) ==> ( root == node || (\valid(root) && reachable(root->next, node)) );
*/
/*@ lemma offset{L1,L2} : 
    \forall int *a, *b, integer begin, end, offset;
         same_elements{L1,L2}(a+offset,b+offset, begin, end) ==>
         same_elements{L1,L2}(a, b, begin+offset, end+offset);
*/
