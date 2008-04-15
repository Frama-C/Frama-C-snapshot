typedef struct { int i; } T;

/*@ lemma toto{L}:
  @  \forall T t; t.i == 0;
  @*/


extern int G;

/*@ global invariant G_const: G == 0; */

static int i;

/*@ global invariant invi: i >= 0; */
