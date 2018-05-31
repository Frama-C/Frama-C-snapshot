#include "__fc_define_file.h"

int t[10];

FILE __fc_fopen[512];
const FILE* _p__fc_fopen = __fc_fopen;

/*@ 
  assigns \result \from filename[..],mode[..], _p__fc_fopen; 
  ensures  \result==\null || \subset(\result, &__fc_fopen[0 .. 512-1]);
*/ 
FILE *fopen(const char * restrict filename,
     const char * restrict mode);

/*@ 
  assigns \result \from filename[..],mode[..], _p__fc_fopen; 
  ensures  \result==\null || (\result \in &__fc_fopen[0 .. 512-1]) ;
*/ 
FILE *fopen2(const char * restrict filename,
     const char * restrict mode);

void main1(int i, int j) {
  //@ assert \subset(&t[i], &t[..]);
  //@ assert !\subset(&t[0..5], &t[1..6]);
  //@ assert i >= 5 ==> !\subset(&t[i], &t[0..4]);

  // assert i == 6 && j == 5;
  if (i >= j) {
    //@ assert \subset(&t[0..j], &t[0..i]);
  }

  FILE* p = fopen ("bla", 0);
  //@ assert p == \null || \valid(p);
  p = fopen2 ("bli", 0);
  //@ assert p == \null || \valid(p);
}

volatile v;

void main2() {
  int x = 1;
  int y = 1;
  int z = x;
  //@ assert \subset(z, \union(x, y)); // Test coercion int -> set<int>
}

void main3() {
  int a[10], b[300];
  a[v] = v;
  b[v] = v;
  //@ assert \initialized(&a[0..9]);
  //@ assert \initialized(&b[0..299]);

  //@ assert \subset(a[0..8], 5); // Reduction succeeds

  int z = v;
  //@ assert -10 <= z <= 10;
  //@ assert \subset(b[0..100], z); // Reduction succeeds
  //@ assert \subset(b[50..260], z/2); // Reduction fails: not enough plevel
}

void main4() {
  int a[10];
  a[v] = v;
  //@ assert \initialized(&a[0..9]);
  //@ assert a[5] == 5;
  int k = v;
  //@ assert 4 <= k <= 8;
  //@ assert \subset(a[0..9], k); // Reduction
  //@ assert \subset(a[0..9], k); //Still not true, because k is not a singleton
  if (v) {
    //@ assert k == 6;
    //@ assert \subset(a[0..4], k); // Reduction
    //@ assert OK: \subset(a[0..4], k); // True
    //@ assert KO: \subset(a[0..9], k); // Reduces to bottom (currently evaluates to true)
  }
}

void main() {
  main1(v, v);
  main2();
  main3();
  main4();
}
