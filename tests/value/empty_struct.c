/* run.config*
   STDOPT: +" -machdep gcc_x86_32 -cpp-extra-args=-DP1 -then -lib-entry"
   STDOPT: +" -machdep gcc_x86_32 -cpp-extra-args=-DP2 -lib-entry"
   STDOPT: +" -machdep gcc_x86_32 -cpp-extra-args=-DP3 -lib-entry"
   STDOPT: +" -cpp-extra-args=-DP1 -lib-entry"
   STDOPT: +" -cpp-extra-args=-DP1 -absolute-valid-range 0-1 -main main2"
   STDOPT: +"  -cpp-extra-args=\"-DP1 -DP5\" -machdep gcc_x86_32 -absolute-valid-range 0-1 -main main3"
*/

// BTS 1416 and 1874

struct s {}; // empty structs only allowed in GCC/MSVC mode
struct s2 { int i1; struct s s; int i2; };

#define S struct s s;
#define S2 struct s2 s2;
#define T struct s t[10];

// Reorder the variables so that we get an error for each one
#ifdef P1
S // direct empty struct
S2
T
#endif

#ifdef P2
S2 // empty struct inside a struct
S
T
#endif

#ifdef P3
T // array of empty struct
S2
S
#endif

void main() {
  void *p = &s;
  s2.s = s;
  t[0] = t[1];
}

#ifdef P4 // Original example of bts 1874. Not explicitly tested, as the core functionality is checked by the tests above
struct lock_class_key {}; /* pas de struct-declaration-list */

struct dentry {
 struct super_block *d_sb;
};

void task_pgrp_nr_ns(struct dentry x);

extern struct dentry a;

struct super_block {
 struct lock_class_key s_writers_key[4];
} task_pgrp_nr(void) {
 task_pgrp_nr_ns(a);
 /* pas de return */
}

#endif

// tests that dereferencing a (invalid) pointer to an empty struct does not
// crash when -valid-absolute-range is set
struct empty {};
void main2(int n) {
  struct empty * ptr_ret = (struct empty *)0x2;
  if (n) *ptr_ret; // invalid access, but should not crash
}

#ifdef P5
#include <stdlib.h>
struct empty empties[100];
volatile int nondet;
void main3(int n) {
  struct empty *q = malloc(0);
  struct empty *r = realloc(q, 0);
  struct empty *p = empties;
  for (int i = 0; i < 100; i++) {
    empties[i] = *r;
  }
  *p = empties[99];
  *p = *r;
  free(r);
}
#endif
