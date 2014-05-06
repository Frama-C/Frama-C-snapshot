/* run.config
STDOPT: +"-remove-unused-specified-functions" +"-kernel-msg-key printer:builtins"
*/
typedef struct { int i; } T;

/*@ lemma toto{L}:
  @  \forall T t; t.i == 0;
  @*/


extern int G;

/*@ global invariant G_const: G == 0; */

static int i;

/*@ global invariant invi: i >= 0; */

extern int c;

/*@ requires c==0; */
void __attribute__((FC_BUILTIN)) foo(int*);
