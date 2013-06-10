/* run.config
   OPT: -rte -warn-signed-overflow -print -rte-precond -journal-disable
*/

int global = 15;

typedef struct cell {
  int val;
  struct cell* next;
} cell;


typedef struct other {
  cell c;
} other;


/*@ requires x > 0 ;
    requires (int) (x + y) != 0 ;
*/
int f(int x, int y, float z) {
  return x + y - (int) z;
}

/*@
  predicate is_valid_int_range(int* p, int n) =
  (0 <= n) && \valid_range(p,0,n-1) ;
*/


/*@ requires is_valid_int_range(p,i);
 */
int g(int* p, int i) {
  if (i >= 1)
    return p[i-1];
  else return 0;
}

/*@ requires \valid(&p[1]+3) ;
 */
int h(int* p) {
  return *(p+3);
}

/*@ requires \valid(p->next);
   // requires \valid(&p->next);
   // requires \valid(&p->val);
    requires \valid(p);
 */
cell* set(cell* p, int v) {
  p->val=v;
  return p->next;
}

/*@ requires \valid(cIn.next);
    requires global > 0;
 */
int next_val(cell cIn) {
  return cIn.next->val;
}

typedef struct top {
  int topv;
  cell* next;
  cell* pred;
} top;


/*@ requires \valid(ptop->next);
 */

cell* top_set(top* ptop, int v) {
  return set(ptop->next, v);
}

/*@ requires \valid(tIn.next);
 */
int top_next(top tIn) {
  return next_val(*tIn.next);
}

/*@
    requires \valid(tab_top);
    requires \valid(&tab_top);
    requires \valid(tab_top[i]->next);
 */
cell* tabtop_set(top* tab_top[], int i, int v) {
  return top_set(tab_top[i], v);
}

int main() {
  int a=2,b=3;
  cell c = { 20 , 0 };
  cell* pc;
  top T;
  top tabT[2];
  top* ptabT[3] = { &T, &T, &T };
  top** pptop;
  other ot = { c };

  void* V;

  int k = 1, l = 1;

  int z= f(2,3,1.0), y = f(b-a,a,2.0);

  int w;

  int tab[4] = { 1, 2, 3, 4 };
  int* r;

  int** rr;

  z = f(y,a+b,-0.0);

  w = g(&z,1);

  w = g(&tab[1],2);

  w = g(&tab[k],l);

  r = tab;

  rr = &r;

  w = g(r+2,0);

  w = h(tab);
  w = h(r);
  w = h (*rr);

  //  w = h(&tab[1]);
  //  w = h(&tab[k]);

  pc = &c;
  c.next = &c;
  set(pc,15);
  set(&c,10);
  set((cell*)(void*)&c,20);

  V = &c;
  set((cell*) V, 20);

  next_val(c);
  next_val(*pc);
  next_val(*((cell*) V));

  T.pred = &c;
  T.next = &c;

  tabT[0] = T;
  tabT[1] = T;

  set(T.pred,10);
  set(tabT[1].next,20);

  next_val(*T.next);
  next_val(*tabT[0].pred);

  set(ptabT[2]->pred,15);
  set(tabT[1].pred,10);
  next_val(*(ptabT[1]->next));

  top_set(&T, 3);
  top_set(&tabT[1],2);
  top_set(ptabT[1],4);
  top_set((void*) (c.next), 5);


  top_next(T);
  top_next(*ptabT[0]);


  tabtop_set(ptabT, 2, 10);
  pptop = (void*) ptabT;

  tabtop_set(pptop,2,15);
  tabtop_set((void*) ptabT, 1, 20);

  tabtop_set((void*) pc->next, 1, 10);

  return z;
}
