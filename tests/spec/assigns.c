struct list {
  int hd;
  struct list *next;
};

/*@ predicate reachable{L}(struct list *root, struct list *to) =
  @   root == to || root != \null && reachable(root->next,to) ;
  @*/
int * q;
//@ assigns *p; assigns *p,q;
void reset(int *p) { *p = 0; }

// three equivalent assigns clauses
//@ assigns t[0..n-1];
void reset_array1(int t[],int n) {
  int i;
  for (i=0; i < n; i++) t[i] = 0;
}

//@ assigns *(t+(0..n-1));
void reset_array2(int t[],int n) {
  int i;
  for (i=0; i < n; i++) t[i] = 0;
}

//@ assigns *(t+{ i | int i ; 0 <= i < n });
void reset_array3(int t[],int n) {
  int i;
  for (i=0; i < n; i++) t[i] = 0;
}

//@ assigns { q->hd | struct list *q ; reachable(p,q) };
void incr_list(struct list *p) {
  while (p) { p->hd++ ; p = p->next; }
}

/*@
  predicate is_empty (set<int *> s) =
  s == \empty;
  @*/
