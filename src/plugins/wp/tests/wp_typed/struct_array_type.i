/* run.config
   OPT: -wp-msg-key print-generated -wp-prover why3 -wp-gen
*/

typedef unsigned int size_t;

struct s {
  int a[10];
  int b[10];
};

struct s t[10];

/*@
  requires 0 <= SynchroId < 10;
  ensures  \result > t[SynchroId].b[0];
  @*/
int f(size_t const SynchroId){
  if( t[SynchroId].a[0] > t[SynchroId].b[0]){
    return 2*t[SynchroId].a[0] - t[SynchroId].b[0] + 1;
  } else {
    return 2*t[SynchroId].b[0] - t[SynchroId].a[0] + 1;
  }
}
