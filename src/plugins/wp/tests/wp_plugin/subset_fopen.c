/* from value test tests/misc/subset.c */
#include "__fc_define_file.h"

int t[10];

FILE __fc_fopen[512];
const FILE* _p__fc_fopen = __fc_fopen;

/*@
  assigns \result \from filename[..],mode[..], _p__fc_fopen;
  ensures  \result==\null || (\subset(\result,&__fc_fopen[0 .. 512-1])) ;
*/
FILE *fopen(const char * restrict filename,
     const char * restrict mode);

void f(int i, int j) {
  //@ assert Ok_A: \subset(&t[i], &t[..]);
  //@ assert Ok_B: !\subset(&t[0..5], &t[1..6]);
  //@ assert Ok_C: i >= 5 ==> !\subset(&t[i], &t[0..4]);

  if (i >= j) {
    //@ assert Ok_D: \subset(&t[0..j], &t[0..i]);
  }

  FILE* p = fopen ("bla", 0);
  //@ assert Ok_E: p != \null ==> \valid(p);
}
