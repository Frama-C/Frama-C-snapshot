/* run.config
 OPT: -print -journal-disable -continue-annot-error
*/

// test of label inference into 
typedef struct {
  int a;
  unsigned char *b;
} S ;

typedef struct {
  int a;
  unsigned char b[10];
} T ;

int * P ;
int V, Tab[10] ;

/*@ axiomatic A {

    logic T t reads \nothing;
    logic integer ft (T x) = x.b[1] ;
    logic integer ft2 (T x) reads x.b[1] ;

    predicate PT(T x) = x.b[1] > 1;
    predicate PT2(T x) reads x.b[1];

    axiom T1: t.a == 0;
    axiom T2: t.b[0] == 0;
    axiom T3: PT(t) ==> PT2(t);

    logic S s reads \nothing;
    logic integer fs (S x) = x.b[1] ;      // <- label to infer
    logic integer fs2 (S x) reads x.b[1] ; // <- label to infer

    predicate PS(S x) = x.b[1] > 1;        // <- label to infer
    predicate PS2(S x) reads x.b[1];       // <- label to infer

    axiom S1: s.a == 0;
    axiom S2: s.b[0] == 0;      // <- label to infer
    axiom S3: PS(s) ==> PS2(s); // <- label to infer

    logic integer p reads *P;   // <- label to infer
    logic int  * va reads &V;   // <- label to infer
    logic integer v reads V;    // <- label to infer
    logic int *fa(int *q) = q;
    logic int fa2(int *q) = *q; // <- label to infer
    logic char *fa3(integer i) = i + (char *)&P;   // <- label to infer
    logic integer fa4(T *q) reads q->a ;           // <- label to infer
 
    predicate Initialized(int *q) = \initialized(q);      // <- label to infer
    predicate Valid(int *q) = \valid(q);                  // <- label to infer
    predicate ValidIndex(int *q) = \valid_index(q,1);     // <- label to infer
    predicate ValidRange(int *q) = \valid_range(q,0,2);   // <- label to infer
    predicate Separated(int *a,int *b) = \separated(a,b);

    logic integer BlockLength(int *q) = \block_length(q); // <- label to infer
    logic char * Base_addr(int *q) = \base_addr(q);       // <- label to infer

//    logic integer Offset(int *q) = \offset(q);          // <- BUG parsing 

    logic integer fi(T* t) = t->a + (*t).a;

}
*/


typedef struct _list { 
  int element;
  struct _list* next; } list;

/*@ type List<A> = Nil | Cons(A,List<A>); */

/*@ inductive model_0{L1,L2}(list* root, List<int>logic_list) {
case nil{L1,L2}: model_0{L1,L2}(\null,Nil);
case cons{L1,L2}: \forall list* l1,List<int>ll1; 
\at(\valid(l1),L1) ==> model_0{L1,L2}(\at(l1->next,L1),ll1) ==> 
model_0{L1,L2}(l1,Cons(\at(l1->element,L1),ll1));
} */

/*@ inductive model_1{L}(list* root, List<int>logic_list) {
case nil{L}: model_1{L}(\null,Nil);
case cons{L}: \forall list* l1,List<int>ll1; \valid(l1) ==>
model_1{L}(\at(l1->next,L),ll1) ==> model_1{L}(l1,Cons(\at(l1->element,L),ll1));
} */

/*@ inductive model_2(list* root, List<int>logic_list) {
case nil: model_2(\null,Nil);
case cons: \forall list* l1,List<int>ll1; \valid(l1) ==>
model_2(l1->next,ll1) ==> model_2(l1,Cons(l1->element,ll1));
} */

/*@ inductive model_3{L}(list* root, List<int>logic_list) {
case nil: model_3(\null,Nil);
case cons{L}: \forall list* l1,List<int>ll1; \valid(l1) ==>
model_3(l1->next,ll1) ==> model_3(l1,Cons(l1->element,ll1));
} */

/*@ inductive model_4{L1}(list* root, List<int>logic_list) {
case nil: model_4(\null,Nil);
case cons{L1}: \forall list* l1,List<int>ll1; \valid(l1) ==>
model_4(l1->next,ll1) ==> model_4(l1,Cons(l1->element,ll1));
} */

/*@ inductive model_5{L}(list* root, List<int>logic_list) {
case nil: model_5(\null,Nil);
case cons{L1}: \forall list* l1,List<int>ll1; \valid(l1) ==>
model_5(l1->next,ll1) ==> model_5(l1,Cons(l1->element,ll1));
} */

/*@ inductive model_6(list* root, List<int>logic_list) {
case nil{L1}: model_6(\null,Nil);
case cons: \forall list* l1,List<int>ll1; \valid(l1) ==>
model_6(l1->next,ll1) ==> model_6(l1,Cons(l1->element,ll1));
} */

/*@ inductive model_7{L1}(list* root, List<int>logic_list) {
case nil{L1}: model_7(\null,Nil);
case cons: \forall list* l1,List<int>ll1; \valid(l1) ==>
model_7(l1->next,ll1) ==> model_7(l1,Cons(l1->element,ll1));
} */

/*@ inductive model_8{L1}(list* root, List<int>logic_list) {
case nil{L}: model_8(\null,Nil);
case cons: \forall list* l1,List<int>ll1; \valid(l1) ==>
model_8(l1->next,ll1) ==> model_8(l1,Cons(l1->element,ll1));
} */

/*@ inductive model_9{L1,L2}(list* root, List<int>logic_list) {
case nil: \valid(P);
} */

/* inductive model_10{L}(list* root, List<int>logic_list) {
case nil: model_10(\null,Nil);
case cons{L}: \forall list* l1,List<int>ll1; \valid{L}(l1) ==>
model_10(l1->next,ll1) ==> model_10(l1,Cons(l1->element,ll1));
}
*/
