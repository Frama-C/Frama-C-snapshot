/* run.config
   STDOPT: #" -cpp-extra-args=-DP1" +"-then -lib-entry"
   STDOPT: #" -cpp-extra-args=-DP2 -lib-entry"
   STDOPT: #" -cpp-extra-args=-DP3 -lib-entry"
*/

// BTS 1416 and 1874

struct s {};
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

void main();
#endif
