typedef int foo[4];

foo X = {0,1,2,3};

/*@ predicate p1(int* a) = \valid_range(a,0,3); */

/*@ predicate q1(int* a) = \valid(a + (0..3)); */

// [VP] Incompatible with the idea that foo denotes a logic array which can
// not be seen as a pointer.
/* @ lemma tauto: \forall foo a; p1(a) <==> q1(a); */

// There's a slight difference between the two following lemmas: the first
// one speaks of the address of X, while the second speaks of the address of the
// first element of X (the type is different in particular)

/*@ lemma vaddrof: \valid(&X); */

/*@ lemma vaddrof2: \valid(&X[0]); */

/*@ lemma tauto1{L}: q1(X); */

/*@ requires p1(x); */
int f1(foo x) { return x[3]; }

int g1() { return f1(X); }

//@ axiomatic ax { logic boolean p{L}(int *b); }

int a[10];

void ftest(void) {
    //@ assert p(&a[0]);
    //@ assert p( a   );
}


//@ lemma array_not_null: a != \null; 
