/* run.config*
   STDOPT: +" -eva-warn-copy-indeterminate=-cast_address -eva -out -input -deps -calldeps -eva-msg-key initial-state -absolute-valid-range 100-200 -inout -then -lib-entry"
 */
#include "string.h"
struct s;
//@ assigns *p \from \nothing;
void f(struct s *p);

void invalid_assigns_imprecise() {
  struct s *p = 0;
  f(p); // p is invalid, but could be considered valid since sizeof(*p) = Top
}

void write_garbled() { // Write through a garbled mix
  int i = 1;
  int j = 2;
  int k[5] = { 2, 3};

  int *p = (&j + (int) &k) - (int) &k; // creates a garbled mix
  *p = 1;
  Frama_C_dump_each();
  *p = (int) p;
}

volatile int v, addr;

struct s v1, v2;
struct u v3, v5;
struct s* t[10];




void abstract_structs() {
  char *p = &v1;
  if (v) {
    char w1 = *p+1;
  }
  if (v) {
    char w = *p;
  }
  //if (v) {
  //  struct s v4 = v1; // this is now a syntax error
  //}
  *p = 1;
  char q = *p;
//  if (v) {
//    v1 = v2; // Illegal according to C standard (assigning incomplete type)
//  }
//  v2 = v1;
  memset(&v3, -5, sizeof(v3)); // Also illegal, rejected by gcc
  int *p2 = ((int*)&v2)+1;
  *p2 = (int) &addr;
//  *t[5] = v2; // assigning incomplete type
  char *p4 = ((char*)&v5) + (short)v;
  *p4 = 18;
  char *p5 = ((char*)&v5) + (signed int)v;
  *p5 = 19;
  char *p6 = ((char*)&v5) + (unsigned int)v;
  *p6 = 20;
}

void cast_address() {
  int x;
  int *p = &x;
  char c1 = (char) p;
  char c2 = *((char*)&p);
  char c3 = *((char*)&p)+0;
}

int **p_gm_null;
void * gm_f1 (void);

void garbled_mix_null () {
  p_gm_null = gm_f1();
  Frama_C_dump_each();
  gm_f2(*p_gm_null); // Corrupts all the null base
  Frama_C_dump_each(); // Joining the state with the previous call to
                       // Frama_C_dump_each may cause a crash is the offsetmap
                       // for NULL is invalid
}

struct s s1, s2;

void weak_update_imprecise_size() {
  // assigning to incomplete type: UB, rejected by gcc
  // s1 = s2; // Must not be considered a strong update
  int vx = 1;
  int vy;
  if (v) {
    vy = 1;
  } // Test 'link' on  a Top range (when the effect of 's1 = s2' is erroneously
    // computed
}

struct u1 {
  int i1;
  short i2;
};

#pragma pack (1)
struct u2 {
  int i1;
  short i2;
};

void many_writes() {
  struct u1 t_packed1[300];
  struct u2 t_packed2[300];

  t_packed1[v].i1 = 1;
  //@ assert t_packed1[3].i1 == 1;

  t_packed2[v].i1 = 1; // Due to packing, we cannot simply write '1' each
                      // sizeof(int)
  //@ assert t_packed2[3].i1 == 1;

}

void overlap () {
  char t_char[100];
  *(int *)((char*) t_char + v) = 1; // The write overlap, we must approximate
}

void paste_big () {
  struct s_big {
    char c [20480];
    int i;
  };

  unsigned int i = v;
  struct s_big s;
  memset(&s, 2, sizeof(s));

  struct s_big t_big[300];
  //@ assert i < 300;
  t_big[i] = s;  // This triggers an imprecise copy (300 > plevel). Make sure
                 // we do not read the struct as a precise integer, this is
                 // too slow
  int v = * ((int *)&t_big + 48) ;
}

void main() {
  invalid_assigns_imprecise();
  write_garbled();
  abstract_structs();
  cast_address();
  garbled_mix_null();
  weak_update_imprecise_size();
  many_writes();
  overlap();
  paste_big ();
}
