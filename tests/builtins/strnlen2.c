#include "__fc_builtin.h"
#include "string.h"

static volatile int nondet;
#define assert_bottom(exp,id) if (nondet) { exp; Frama_C_show_each_unreachable_ ## id(); }


const char* static_str = "Hello World\n";
const char* zero_str = "abc\0\0\0abc";
#define TSZ 12

// Definitions for C++ oracle checking
typedef int Ival;
 typedef int RES;
#define NONDET(a,b) (nondet ? (a) : (b))
#define IF_NONDET(var,val) if (nondet) var = val
#define RANGE(from,to) Frama_C_interval(from,to)
#define STRNLEN(base,offs,n) strnlen(base+offs,n)
#define CHAR_ARRAY(var,n) char var[n]
#define CHAR_PTR(var) char *var
#define STRING(var,str) var = str;

const char* tab_str[TSZ] = 
  {
    "" , // 0
    "a", // 1
    "aa" , // 2
    "aaa" , // 3
    "aaaa" , // 4
    "aaaaa" , // 5
    "aaaaaa" , // 6
    /* hole */
    "aaaaaaaaa" , // 9
    "aaaaaaaaaa" ,
    "aaaaaaaaaaa",
    "aaaaaaaaaaaa" ,
    "aaaaaaaaaaaaa" }; // 13

char unterminated_string[12] = "unterminated";

void small_sets() {
  CHAR_PTR(s);
  STRING(s,"abc");
  Ival o = NONDET(0, 1);
  RES z1 = STRNLEN(s, o, 3);
  //@ assert (z1 == 2 || z1 == 3);
  STRING(s,"\0bc");
  o = NONDET(0,1);
  RES z2 = STRNLEN(s, o, 2);
  //@ assert (z2 == 0 || z2 == 2);
  STRING(s,"");
  RES z3 = STRNLEN(s, 0, 0);
  //@ assert (z3 == 0);
  STRING(s,"b\0c");
  o = NONDET(0,2);
  RES z4 = STRNLEN(s, o, 2);
  //@ assert (z4 == 1);
  CHAR_ARRAY(t, 4);
  t[0] = t[1] = NONDET(0, 1);
  t[2] = t[3] = 1;
  RES z5 = STRNLEN(t, 0, 4); // no warning
  //@ assert z5 == 0 || z5 == 1 || z5 == 4;
}

void zero_termination() {
  CHAR_ARRAY(empty_or_non_terminated, 1);
  empty_or_non_terminated[0] = NONDET(0, 100);
  RES z1 = STRNLEN(empty_or_non_terminated, 0, 1);
  //@ assert z1 == 0 || z1 == 1;

  CHAR_ARRAY(non_terminated, 1);
  non_terminated[0] = 'X';
  assert_bottom(STRNLEN(non_terminated, 0, 2), non_terminated);

  CHAR_ARRAY(non_terminated2, 4);
  non_terminated2[0] = 76; non_terminated2[1] = 0;
  non_terminated2[2] = 28; non_terminated2[3] = 14;
  Ival o = NONDET(2, 3);
  assert_bottom(STRNLEN(non_terminated2, o, 4), non_terminated2);
}

void initialization() {
  char empty_or_uninitialized[1];
  IF_NONDET(empty_or_uninitialized[0], 0);
  RES z1 = strnlen(empty_or_uninitialized, 1);
  //@ assert (z1 == 0);

  char uninitialized[1];
  assert_bottom(strnlen(uninitialized, 1),uninitialized);

  CHAR_ARRAY(s, 2);
  IF_NONDET(s[0], 1);
  s[1] = 0;
  RES z2 = strnlen(s, 2);
  //@ assert (z2 == 1);

  CHAR_ARRAY(t,4);
  t[0] = t[1] = 10;
  IF_NONDET(t[2], 10);
  t[3] = 0;
  RES z3 = strnlen(t, 4);
  //@ assert (z3 == 3);
}

typedef struct {
  int a:8;
  int b:8;
  int c:17;
} st;

void bitfields() {
  st s;
  s.a = 3;
  s.b = 1;
  s.c = 7;
  char *p = &s;
  assert_bottom(strnlen(p, 3),bitfields);
}

typedef struct {
  int a:4;
  int b:4;
  int c:17;
} st2;

void bitfields2() {
  st2 s;
  s.a = 3;
  s.b = 1;
  s.c = 7;
  char *p = &s;
  RES z1 = strnlen(p, 3);
  //@assert (z1 == 2);
}

void init_array_nondet(void *a, int from, int to, int val1, int val2) {
  int val = nondet ? val1 : val2;
  memset(((char*)a) + from, val, to-from+1);
  from = to = val1 = val2 = -1; // reset to minimize oracle changes
}

void large() {
  char a[100];
  init_array_nondet(a, 0, 99, 1, 2);
  a[20] = 0;
  a[75] = 0;
  Ival offset = RANGE(3, 30);
  RES z1 = STRNLEN(a, offset, 100);
  //@ assert (z1 >= 0 && z1 <= 54);

  offset = RANGE(5, 17);
  RES z2 = STRNLEN(a, offset, 100);
  //@ assert (z2 >= 3 && z2 <= 15);

  offset = RANGE(60, 74);
  RES z3 = STRNLEN(a, offset, 100);
  //@ assert (z3 >= 1 && z3 <= 15);

  offset = RANGE(63, 80);
  RES z4 = STRNLEN(a, offset, 100);
  //@ assert (z4 >= 0 && z4 <= 12);

  init_array_nondet(a, 0, 99, 0, 2);
  offset = RANGE(50, 70);
  RES z5 = STRNLEN(a, offset, 100);
  //@ assert (z5 >= 0 && z5 <= 49);
}

void large_uninit() {
  char a[100];
  init_array_nondet(a, 0, 39, 1, 2);
  init_array_nondet(a, 50, 94, 3, 4);
  a[20] = 0;
  a[75] = 0;

  Ival offset = RANGE(3, 30);
  RES z1 = STRNLEN(a, offset, 100);
  //@ assert (z1 >= 0 && z1 <= 17);

  a[98] = 0;
  offset = RANGE(63, 80);
  RES z2 = STRNLEN(a, offset, 100);
  //@ assert (z2 >= 0 && z2 <= 12);

  offset = RANGE(45, 55);
  RES z3 = STRNLEN(a, offset, 100);
  //@ assert (z3 >= 20 && z3 <= 25);

  offset = 0; // avoid oracle diffs when changed
}

void escaping() {
  CHAR_ARRAY(s,4);
  {
    int x;
    *((int *)s) = (int) &x;
  }
  IF_NONDET(s[0], 0);
  RES z1 = strnlen(s, 4); // alarm
  //@ assert (z1 == 0);
  s[0] = 0;
  RES z2 = strnlen(s, 4); // no alarm
  //@ assert (z2 == 0);
}

void misc() {
  const char* loc_str = "Bonjour Monde\n";
  char loc_char_array[5];
  size_t sz1,sz2,sz3,sz4,sz5,sz6,sz7,sz8;
  int x = 0xabcdef00;
  RES z = 0x12345600;
  int i;
  char *str;

  assert_bottom(strnlen(unterminated_string, 13),unterminated_string);

  str = nondet ? static_str : loc_str;
  sz1 = strnlen(str, 14);  
  //@ assert(sz1 == 12) || (sz1 == 14);
  str = &x;
  str = nondet ? str : str + 3;
  sz2 = strnlen(str, 12);
  //@ assert(sz2 == 0) ; // no, could also do an RTE
  i = Frama_C_interval(0,TSZ-1);
  str = tab_str[i];
  sz3 = strnlen(str, 13);
  //@ assert (sz3 >= 0) && (sz3 <= 13);
  loc_char_array[3] = '\0';
  assert_bottom(strnlen(loc_char_array, 5),loc_char_array);
  sz4 = strnlen(zero_str, 9);
  //@ assert(sz4 == 3);
  char *s1 = nondet ? "abc" : "ABCD";
  char *s2 = nondet ? s1 : s1+1;
  sz5 = strnlen(s2, 5);
  //@ assert(sz5 >= 2 && sz5 <= 4);

  s1 = nondet ? "efg" : "EFGH";
  s2 = nondet ? s1+1 : s1+2;
  sz6 = strnlen(s2, 5);
  //@ assert(sz6 >= 1 && sz6 <= 3);

  s1 = nondet ? "mno\0pqr" : "MNOP\0QRS";
  for (int j = 0; j < 8; j++) {
    sz7 = strnlen(s1 + j, 10);
    //@ assert(sz7 >= 0 && sz7 <= 4);
  }

  char maybe_init[2];
  maybe_init[1] = '\0';
  IF_NONDET(maybe_init[0], 'A');
  sz8 = strnlen(maybe_init, 2);
  //@ assert(sz8 == 1);
}

void big_array () {
  int u[200];
  int r[201];
  int t[1000000];
  int *p;
  p = &u[nondet];
  *p = 0x10230067;
  p = &r[nondet];
  *p = 0x10230067;
  p = &t[nondet];
  *p = 0x10230067;

  unsigned long len_u;
  unsigned long len_r;
  unsigned long len_t;

  // without initialization, most accesses are invalid, so the result is precise
  len_u = strnlen((char const *)u, 800); // below plevel; precise
  len_r = strnlen((char const *)r, 804); // above plevel; imprecise
  len_t = strnlen((char const *)t, 4000000); // *far* above plevel
  //@ assert len_u == 1;
  //@ assert len_r == 1;
  //@ assert len_t == 1;
  Frama_C_show_each(len_u, len_r, len_t);

  // less precise results here, but uninitialized values at the end of the
  // arrays ensure a slightly better result than afterwards
  len_u = strnlen((char const *)(u+nondet),801); // should be precise
  len_r = strnlen((char const *)(r+nondet),805);
  len_t = strnlen((char const *)(t+nondet),4000001);
  //@ assert len_u >= 0 && len_u <= 3;
  //@ assert len_r >= 0 && len_r <= 800;
  //@ assert len_t >= 0 && len_t <= 3999996;
  Frama_C_show_each(len_u, len_r, len_t);

  // initialization leads to less precise results
  init_array_nondet(u, 0, sizeof(u)-1, 0, 1);
  init_array_nondet(r, 0, sizeof(r)-1, 0, 1);
  init_array_nondet(t, 0, sizeof(t)-1, 0, 1);
  p = &u[nondet];
  *p = 0x10230067;
  p = &r[nondet];
  *p = 0x10230067;
  p = &t[nondet];
  *p = 0x10230067;
  len_u = strnlen((char const *)u, 800); // below plevel; precise
  len_r = strnlen((char const *)r, 804); // above plevel; imprecise
  len_t = strnlen((char const *)t, 4000000); // *far* above plevel
  //@ assert len_u >= 0 && len_u <= 800;
  //@ assert len_r >= 0 && len_r <= 804;
  //@ assert len_t >= 0 && len_t <= 4000000;
  Frama_C_show_each(len_u, len_r, len_t);

  len_u = strnlen((char const *)(u+nondet),801); // should be precise
  len_r = strnlen((char const *)(r+nondet),805);
  len_t = strnlen((char const *)(t+nondet),4000001);
  //@ assert len_u >= 0 && len_u <= 799;
  //@ assert len_r >= 0 && len_r <= 803;
  //@ assert len_t >= 0 && len_t <= 3999999;
  Frama_C_show_each(len_u, len_r, len_t);


}

void no_zero_but_ok() {
  CHAR_ARRAY(s,20);
  s[0] = s[1] = s[2] = s[3] = s[4] = s[5] = s[6] = s[7] = s[8] = s[9] = 1;
  s[10] = 0;
  s[11] = s[12] = s[13] = 1;
  s[14] = s[15] = s[16] = s[17] = s[18] = s[19] = nondet ? 1 : 0;
  RES z1 = strnlen(s, 5);
  //@assert z1 == 5;
  RES z2 = strnlen(s, 10);
  //@assert z2 == 10;
  char *p = nondet ? s+1 : s+8;
  RES z3 = strnlen(p, 10);
  //@assert z3 == 2 || z3 == 9;
  p = nondet ? s+7 : s+11;
  RES z4 = strnlen(p,4);
  //@assert z4 == 3 || z4 == 4;
  p = nondet ? s+7 : s+18;
  RES z5 = strnlen(p,4); // maybe_indet = true
  //@assert z5 == 0 || z5 == 1 || z5 == 3;
}

void small_sets_n() {
  char *s;
  STRING(s,"abcde");
  char *p = nondet ? s : s+1;
  char n = nondet ? 2 : 5;
  RES z1 = strnlen(p, n);
  //@assert z1 == 2 || z1 == 4 || z1 == 5;

  STRING(s,"\0bcdef");
  p = nondet ? s : s+1;
  n = nondet ? 1 : 4;
  RES z2 = strnlen(p, n);
  //@assert z2 == 0 || z2 == 1 || z2 == 4;

  STRING(s,"bcd\0efg");
  p = nondet ? s : s+2;
  RES z3a = strnlen(p, 2);
  //@assert z3a == 1 || z3a == 2;

  p = nondet ? s : s+2;
  n = nondet ? 2 : 3;
  RES z3 = strnlen(p, n);
  //@assert z3 == 1 || z3 == 2 || z3 == 3;

  p = nondet ? s : nondet ? s+2 : s+4;
  n = nondet ? 2 : 5;
  RES z4 = strnlen(p, n);
  //@assert z4 == 1 || z4 == 2 || z4 == 3;
}

void large_n() {
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 99, 1, 2);
  a[15] = 0;
  a[28] = 0;
  a[40] = 0;
  a[75] = 0;
  Ival offset = RANGE(3, 30);
  Ival n = RANGE(10, 20);
  RES z1 = STRNLEN(a, offset, n);
  //@assert z1 <= 12;

  a[28] = 1;
  a[29] = 0;
  RES z2 = STRNLEN(a, offset, n);
  //@assert z2 <= 13;

  a[40] = 1;
  RES z3 = STRNLEN(a, offset, n); // no alarm
  //@assert z3 <= 20;

  offset = RANGE(5, 17);
  RES z4 = STRNLEN(a, offset, n);
  //@assert z4 <= 13;

  offset = RANGE(60, 74);
  RES z5 = STRNLEN(a, offset, n);
  //@assert z5 >= 1 && z5 <= 15;

  offset = RANGE(63, 80);
  RES z6 = STRNLEN(a, offset, n); // no alarm
  //@assert z6 <= 20;

  init_array_nondet(a, 0, 99, 0, 2);
  offset = RANGE(50, 70);
  RES z7 = STRNLEN(a, offset, n);
  //@assert z7 <= 20;

  n = RANGE(0, 100);
  RES z8 = STRNLEN(a, offset, n); // alarm
  //@assert z8 <= 50;

  offset = RANGE(0, 10);
  n = RANGE(0, 90);
  RES z9 = STRNLEN(a, offset, n); // no alarm
  //@assert z9 <= 90;
}

void unbounded_n() {
  int n = nondet;
  if (n < 0) n = 0;
  char *s;
  STRING(s,"abc");
  RES zu1 = strnlen(s, n);
  //@ assert 0 <= zu1 <= 3;
  Ival o = NONDET(0,1);
  RES zu2 = strnlen(s+o, n);
  //@ assert 0 <= zu2 <= 3;
  STRING(s,"bcd\0eg");
  RES zu3 = strnlen(s, n);
  //@ assert 0 <= zu3 <= 3;
}

void intervals() {
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 9, 0, 1);
  init_array_nondet(a, 3, 6, 1, 1);
  init_array_nondet(a, 11, 11, 0, 1);
  init_array_nondet(a, 12, 15, 1, 1);
  init_array_nondet(a, 16, 19, 0, 1);

  Ival offset = RANGE(0,9);
  Ival n = RANGE(0, 9);
  RES z1 = STRNLEN(a, offset, n);
  //@ assert z1 >= 0 && z1 <= 9;

  offset = RANGE(3,9);
  n = RANGE(2,10);
  RES z2 = STRNLEN(a, offset, n);
  //@ assert z2 >= 0 && z2 <= 7;

  offset = RANGE(3,9);
  n = RANGE(0,11);
  RES z3 = STRNLEN(a, offset, n);
  //@ assert z3 >= 0 && z3 <= 7;

  offset = RANGE(3,10);
  n = RANGE(0,9);
  RES z4 = STRNLEN(a, offset, n);
  //@ assert z4 >= 0 && z4 <= 7;

  offset = RANGE(3,10);
  n = RANGE(0,10);
  RES z5 = STRNLEN(a, offset, n);
  //@ assert z5 >= 0 && z5 <= 7;

  offset = RANGE(3,10);
  n = RANGE(0,11);
  RES z6 = STRNLEN(a, offset, n);
  //@ assert z6 >= 0 && z6 <= 7;

  offset = RANGE(3,11);
  n = RANGE(0,9);
  RES z7 = STRNLEN(a, offset, n);
  //@ assert z7 >= 0 && z7 <= 9;

  offset = RANGE(3,11);
  n = RANGE(0,10);
  RES z8 = STRNLEN(a, offset, n);
  //@ assert z8 >= 0 && z8 <= 9;

  offset = RANGE(3,11);
  n = RANGE(0,11);
  RES z9 = STRNLEN(a, offset, n);
  //@ assert z9 >= 0 && z9 <= 9;
}

void negative_offsets() {
  char buf[100];
  for (int i = 0; i < 100; i++) buf[i] = 'A'; //avoid memset due to C++ oracles
  buf[99] = 0;
  unsigned len1, len2, len3, len4, len5, len6;
  if (nondet) {
    int offset1 = Frama_C_interval(-10, -8);
    len1 = strnlen(buf + offset1, 100);
  }
  if (nondet) {
    int offset2 = Frama_C_interval(-2, -1);
    len2 = strnlen(buf + offset2, 100);
  }
  int offset3 = Frama_C_interval(-1, -0);
  len3 = strnlen(buf + offset3, 100);
  int offset4 = Frama_C_interval(-1, 2);
  len4 = strnlen(buf + offset4, 100);
  int offset5 = Frama_C_interval(-4, 7);
  len5 = strnlen(buf + offset5, 100);
  int offset6 = Frama_C_interval(-10, 0);
  char *p = buf + offset6;
  len6 = strnlen(p, 100);
  char dest[100 * 2];
}

int main (int c) {
  small_sets();
  zero_termination();

  initialization();
  large();
  large_uninit();
  misc();
  bitfields();
  bitfields2();
  escaping();
  big_array();

  no_zero_but_ok();
  small_sets_n();
  large_n();

  unbounded_n();
  intervals();

  negative_offsets();
  return 0;
}
