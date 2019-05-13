#include "__fc_builtin.h"
#include "string.h"

// NOTE: all unnamed assertions should be valid.
// Imprecise results should be defined using named
// assertions placed after the less restrictive ones,
// so that in the final output the following sentence
// never arises:
//   Assertion got status unknown.
// Only named assertions may remain unknown, e.g.:
//   Assertion 'refined' got status unknown.

static volatile int nondet;
#define assert_bottom(exp,id) if (nondet) { exp; Frama_C_show_each_unreachable_ ## id(); }



// Definitions for C++ oracle checking
typedef int Ival;
typedef int RES;
#define CHAR char
#define NONDET(a,b) (nondet ? (a) : (b))
#define NONDET_STR(a,b) NONDET((a),(b))
#define NONDET_STR2(a,b) NONDET((a),(b))
#define IF_NONDET(var,val) if (nondet) var = val
#define RANGE(from,to) Frama_C_interval(from,to)
#define CHAR_ARRAY(var,n) char var[n]
#define CHAR_PTR(var) char *var
#define STRING(var,str) var = str;

#define TSZ 12
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

#define MEMCHR_no_decl(lv_var, _p, _offs, _c, _n)       \
  do {                                                  \
    const char *_s = (const char*) _p;                  \
    char *_ss = memchr(_s+_offs,_c,_n);                 \
    int _res;                                           \
    if (_ss == 0) {                                     \
      _res = -1;                                        \
    } else {                                            \
      _res = _ss - (char*)_p;                           \
    }                                                   \
    Frama_C_show_each_mymemchr(_res);                   \
    lv_var = _res;                                      \
  } while (0)                                           \

#define MEMCHR(lv_type, lv_var, _p, _offs, _c, _n)      \
  lv_type lv_var;                                       \
  MEMCHR_no_decl(lv_var, _p, _offs, _c, _n)

#define MEMCHR_bottom(s, offs, c, n) assert_bottom(memchr(s+offs,c,n), s)

#define MEMCHR2(lv_type, lv_var, _base, _offs1, _offs2, _c, _n)      \
  lv_type lv_var;                                                       \
  do {                                                                  \
    const char *_p = (const char *) _base;                              \
    _p += NONDET(_offs1, _offs2);                                       \
    char *_ss = memchr(_p,_c,_n);                                       \
    int _res;                                                           \
    if (_ss == 0) {                                                     \
      _res = -1;                                                        \
    } else {                                                            \
      _res = _ss - (char*)_base;                                        \
    }                                                                   \
    Frama_C_show_each_mymemchr2(_res);                                  \
    lv_var = _res;                                                      \
  } while (0)                                                           \

//DELPHIC_TEST
void memchr_small_sets() {
  CHAR c = 0;
  CHAR_PTR(s);
  STRING(s,"abc");
  Ival o = NONDET(0, 1);
  MEMCHR(RES, z1, s, o, c, 4);
  //@ assert (z1 == 3);
  STRING(s,"\0bc");
  o = NONDET(0,1);
  MEMCHR(RES, z2, s, o, c, 3);
  //@ assert (z2 == 0 || z2 == 3);
  STRING(s,"");
  MEMCHR(RES, z3, s, 0, c, 0);
  //@ assert (z3 == -1);
  STRING(s,"b\0c");
  o = NONDET(0,2);
  MEMCHR(RES, z4, s, o, c, 2);
  //@ assert (z4 == 1 || z4 == 3);
  CHAR_ARRAY(t, 4);
  t[0] = t[1] = NONDET(0, 1);
  t[2] = t[3] = 1;
  MEMCHR(RES, z5, t, 0, c, 4); // no warning
  //@ assert (z5 == -1 || z5 == 0 || z5 == 1);
}

//DELPHIC_TEST
void memchr_zero_termination() {
  CHAR c = 0;
  CHAR_ARRAY(empty_or_non_terminated, 1);
  empty_or_non_terminated[0] = NONDET(0, 100);
  MEMCHR(RES, z1, empty_or_non_terminated, 0, c, 1);
  //@ assert (z1 == -1 || z1 == 0);

  CHAR_ARRAY(non_terminated, 1);
  non_terminated[0] = 'X';
  MEMCHR(RES, z2, non_terminated, 0, c, 1);
  //@ assert (z2 == -1);
  MEMCHR_bottom(non_terminated, 0, c, 2);

  CHAR_ARRAY(non_terminated2, 4);
  non_terminated2[0] = 76; non_terminated2[1] = 0;
  non_terminated2[2] = 28; non_terminated2[3] = 14;
  Ival o = NONDET(2, 3);
  MEMCHR_bottom(non_terminated2, o, c, 4);
}

//DELPHIC_TEST
void memchr_initialization() {
  CHAR c = 0;
  CHAR_ARRAY(empty_or_uninitialized, 1);
  IF_NONDET(empty_or_uninitialized[0], 0);
  MEMCHR(RES, z1, empty_or_uninitialized, 0, c, 1); // alarm: uninit
  //@ assert (z1 == 0);

  CHAR_ARRAY(uninitialized, 1);
  MEMCHR_bottom(uninitialized, 0, c, 1);

  CHAR_ARRAY(s, 2);
  IF_NONDET(s[0], 1);
  s[1] = 0;
  MEMCHR(RES, z2, s, 0, c, 2);
  //@ assert (z2 == 1);

  CHAR_ARRAY(t,4);
  t[0] = t[1] = 10;
  IF_NONDET(t[2], 10);
  t[3] = 0;
  MEMCHR(RES, z3, t, 0, c, 4);
  //@ assert (z3 == 3);
}

typedef struct {
  int a:8;
  int b:8;
  int c:17;
} st;

void memchr_bitfields() {
  CHAR c = 0;
  st s;
  s.a = 3;
  s.b = 1;
  s.c = 7;
  CHAR_PTR(p);
  p = &s;
  assert_bottom(memchr(p, c, 3),bitfields);
}

typedef struct {
  int a:4;
  int b:4;
  int c:17;
} st2;

void memchr_bitfields2() {
  CHAR c = 0;
  st2 s;
  s.a = 3;
  s.b = 1;
  s.c = 7;
  CHAR_PTR(p);
  p = &s;
  MEMCHR(RES, z1, p, 0, c, 3);
  //@assert (z1 == 2);
}

void init_array_nondet(char *a, int from, int to, int val1, int val2) {
  int val = NONDET(val1, val2);
  memset(a + from, val, to-from+1);
  from = to = val1 = val2 = -1; // reset to minimize oracle changes
}

//DELPHIC_TEST
void memchr_large() {
  CHAR c = 0;
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 99, 1, 2);
  a[20] = 0;
  a[75] = 0;
  Ival offset = RANGE(3, 30);
  MEMCHR(RES, z1, a, offset, c, 100); // alarm from precondition
  //@ assert (z1 >= -1 && z1 <= 75);
  //@ assert refined: (z1 == 20 || z1 == 75);

  offset = RANGE(5, 17);
  MEMCHR(RES, z2, a, offset, c, 100); // alarm from precondition
  //@ assert (z2 >= -1 && z2 <= 20);
  //@ assert refined: (z2 == 20);

  offset = RANGE(60, 74);
  MEMCHR(RES, z3, a, offset, c, 100); // alarm from precondition
  //@ assert (z3 >= -1 && z3 <= 75);
  //@ assert refined: (z3 == 75);

  offset = RANGE(63, 80);
  MEMCHR(RES, z4, a, offset, c, 100); // alarm from precondition
  //@ assert (z4 >= -1 && z4 <= 79);
  //@ assert refined: (z4 == 75);

  init_array_nondet(a, 0, 99, 0, 2);
  offset = RANGE(50, 70);
  MEMCHR(RES, z5, a, offset, c, 100); // alarm from precondition
  //@ assert (z5 >= -1 && z5 <= 99);
  //@ assert refined: (z5 >= 50 && z5 <= 99);
}

//DELPHIC_TEST
void memchr_large_uninit() {
  CHAR c = 0;
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 39, 1, 2);
  init_array_nondet(a, 50, 94, 3, 4);
  a[20] = 0;
  a[75] = 0;

  Ival offset = RANGE(3, 30);
  MEMCHR(RES, z1, a, offset, c, 100); //alarm: Uninit
  //@ assert (z1 >= -1 && z1 <= 29);
  //@ assert refined: (z1 == 20);

  a[98] = 0;
  offset = RANGE(63, 80);
  MEMCHR(RES, z2, a, offset, c, 100); //alarm: Uninit
  //@ assert (z2 >= -1 && z2 <= 79);
  //@ assert refined: (z2 == 75);

  offset = RANGE(45, 55);
  MEMCHR(RES, z3, a, offset, c, 100); //alarm: Uninit
  //@ assert (z3 >= -1 && z3 <= 75);
  //@ assert refined: (z3 == 75);

  offset = 0; // avoid oracle diffs when changed
}

void memchr_escaping() {
  CHAR c = 0;
  CHAR_ARRAY(s,4);
  {
    int x;
    *((int *)s) = (int) &x;
  }
  IF_NONDET(s[0], 0);
  MEMCHR(RES, z1, s, 0, c, 4); // alarm
  //@ assert (z1 == 0);
  s[0] = 0;
  MEMCHR(RES, z2, s, 0, c, 4); // alarm in precondition
  //@ assert (z2 == 0);
}

void memchr_misc_array() {
  CHAR c = 0;
  Ival i = RANGE(0,TSZ-1);
  MEMCHR(RES, sz3, tab_str[i], 0, c, 14);
  //@ assert (sz3 >= 0 && sz3 <= 13);
}

//DELPHIC_TEST
void memchr_misc() {
  CHAR c = 0;
  static CHAR_PTR(static_str);
  STRING(static_str, "Hello World\n");
  static CHAR_PTR(zero_str);
  STRING(zero_str, "abc\0\0\0abc");
  CHAR_PTR(loc_str);
  STRING(loc_str, "Bonjour Monde\n");
  CHAR_ARRAY(loc_char_array, 5);
  int x = 0xabcdef00;
  CHAR_PTR(str);
  CHAR_ARRAY(unterminated_string, 12);
  unterminated_string[0] = 'u';
  unterminated_string[1] = 'n';
  unterminated_string[2] = 't';
  unterminated_string[3] = 'e';
  unterminated_string[4] = 'r';
  unterminated_string[5] = 'm';
  unterminated_string[6] = 'i';
  unterminated_string[7] = 'n';
  unterminated_string[8] = 'a';
  unterminated_string[9] = 't';
  unterminated_string[10] = 'e';
  unterminated_string[11] = 'd';

  MEMCHR_bottom(unterminated_string, 0, c, 13);

  str = NONDET_STR2(static_str, loc_str);
  MEMCHR(RES, sz1, str, 0, c, 15); // false alarm (pointers from 2 bases)
  //@ assert (sz1 == 12 || sz1 == 14);
  str = &x;
  MEMCHR2(RES, sz2, str, 0, 3, c, 12); // alarm in precondition
  //@ assert (sz2 == 0); // alarm
  loc_char_array[3] = '\0';
  MEMCHR_bottom(loc_char_array, 0, c, 5); // alarm in precondition
  MEMCHR(RES, sz4a, zero_str, 0, c, 9); // no alarm
  //@ assert (sz4a == 3);
  MEMCHR(RES, sz4b, zero_str, 4, c, 9); // alarm in precondition
  //@ assert (sz4b == 4);
  MEMCHR(RES, sz4c, zero_str, 5, c, 9); // alarm in precondition
  //@ assert (sz4c == 5);
  MEMCHR(RES, sz4d, zero_str, 6, c, 9); // alarm in precondition
  //@ assert (sz4d == 9);
}

//DELPHIC_TEST
void memchr_misc2() {
  CHAR  c = 0;
  CHAR_PTR(s1);
  s1 = NONDET_STR("abc", "ABCD");
  MEMCHR2(RES, sz5, s1, 0, 1, c, 5); // alarm in precondition
  //@ assert (sz5 == 3 || sz5 == 4);

  s1 = NONDET_STR("efg", "EFGH");
  MEMCHR2(RES, sz6, s1, 1, 2, c, 5); // alarm in precondition
  //@ assert (sz6 == 3 || sz6 == 4);

  s1 = NONDET_STR("mno\0pqr", "MNOP\0QRS");
  for (int j = 0; j < 8; j++) {
    MEMCHR(RES, sz7, s1, j, c, 10); // alarm in precondition
    //@ assert (sz7 == 3 || sz7 == 4 || sz7 == 7 || sz7 == 8);
  }

  CHAR_ARRAY(maybe_init, 2);
  maybe_init[1] = '\0';
  IF_NONDET(maybe_init[0], 'A');
  MEMCHR(RES, sz8, maybe_init, 0, c, 2); // alarm
  //@ assert (sz8 == 1);
}

void memchr_big_array () {
  CHAR c = 0;
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
  // All memchr calls in this function emit alarms due to preconditions.
  // The actual length is unsigned, but due to our representation
  // of NULL via -1, we actually use it as signed
  long len_u;
  long len_r;
  long len_t;

  MEMCHR_no_decl(len_u, u, 0, c, 800); // below plevel; precise
  MEMCHR_no_decl(len_r, r, 0, c, 805); // above plevel; imprecise
  MEMCHR_no_decl(len_t, t, 0, c, 4000001); // *far* above plevel
  Frama_C_show_each(len_u, len_r, len_t);
  //@ assert len_u == 1;
  //@ assert len_r >= 1 && len_r <= 801;
  //@ assert len_t >= 1 && len_t <= 3999997;

  MEMCHR_no_decl(len_u, u, 0, c, 1600); // should be precise
  MEMCHR_no_decl(len_r, r, 0, c, 1608);
  MEMCHR_no_decl(len_t, t, 0, c, 8000000);
  Frama_C_show_each(len_u, len_r, len_t);
  //@ assert len_u >= 0 && len_u <= 3;
  //@ assert len_r >= 0 && len_r <= 802;
  //@ assert len_t >= 0 && len_t <= 3999998;
}

//DELPHIC_TEST
void memchr_no_zero_but_ok() {
  CHAR c = 0;
  CHAR_ARRAY(s,20);
  s[0] = s[1] = s[2] = s[3] = s[4] = s[5] = s[6] = s[7] = s[8] = s[9] = 1;
  s[10] = 0;
  s[11] = s[12] = s[13] = 1;
  s[14] = s[15] = s[16] = s[17] = s[18] = s[19] = NONDET(1, 0);
  MEMCHR(RES, z1, s, 0, c, 5);
  //@assert (z1 == -1);
  MEMCHR(RES, z2, s, 0, c, 11);
  //@assert (z2 == 10);
  MEMCHR2(RES, z3, s, 1, 8, c, 11);
  //@assert (z3 == 10);
  MEMCHR2(RES, z4, s, 7, 11, c, 4);
  //@assert (z4 == -1 || z4 == 10 || z4 == 14);
  MEMCHR2(RES, z5, s, 7, 18, c, 5); // maybe_indet = true
  //@assert (z5 == 10 || z5 == 18 || z5 == 19);
}

//DELPHIC_TEST
void memchr_small_sets_n() {
  CHAR c = 0;
  CHAR_PTR(s);
  STRING(s,"abcde");
  CHAR n = NONDET(2, 5);
  MEMCHR2(RES, z1, s, 0, 1, c, n);
  //@assert (z1 == -1 || z1 == 5);

  STRING(s,"\0bcdef");
  n = NONDET(1, 6);
  MEMCHR2(RES, z2, s, 0, 1, c, n);
  //@assert (z2 == -1 || z2 == 0 || z2 == 6);

  STRING(s,"bcd\0efg");
  MEMCHR2(RES, z3a, s, 0, 2, c, 4);
  //@assert (z3a == 3);

  n = NONDET(3, 4);
  MEMCHR2(RES, z3b, s, 0, 2, c, n);
  //@assert (z3b == -1 || z3b == 3);

  n = NONDET(2, 3);
  MEMCHR2(RES, z3, s, 0, 2, c, n);
  //@assert (z3 == -1 || z3 == 3);

  n = NONDET(2, 7);
  MEMCHR2(RES, z4, s, 2, 4, c, n); // alarm
  //@assert (z4 == -1 || z4 == 3 || z4 == 7);
}

//DELPHIC_TEST
void memchr_large_n() {
  CHAR c = 0;
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 99, 1, 2);
  a[15] = 0;
  a[28] = 0;
  a[40] = 0;
  a[75] = 0;
  Ival offset = RANGE(3, 30);
  Ival n = RANGE(10, 20);
  MEMCHR(RES, z1, a, offset, c, n);
  //@assert (z1 >= -1 && z1 <= 40);
  //@assert refined: (z1 == -1 || z1 == 15 || z1 == 28 || z1 == 40);

  a[28] = 1;
  a[29] = 0;
  MEMCHR(RES, z2, a, offset, c, n);
  //@assert (z2 >= -1 && z2 <= 40);
  //@assert refined: (z2 == -1 || z2 == 15 || z2 == 29 || z2 == 40);

  a[40] = 1;
  MEMCHR(RES, z3, a, offset, c, n); // no alarm
  //@assert (z3 >= -1 && z3 <= 29);
  //@assert refined: (z3 == -1 || z3 == 15 || z3 == 29);

  offset = RANGE(5, 17);
  MEMCHR(RES, z4, a, offset, c, n);
  //@assert (z4 >= -1 && z4 <= 29);
  //@assert refined: (z4 == -1 || z4 == 15 || z4 == 29);

  offset = RANGE(60, 74);
  MEMCHR(RES, z5, a, offset, c, n);
  //@assert (z5 >= -1 && z5 <= 75);
  //@assert refined: (z5 == -1 || z5 == 75);

  offset = RANGE(63, 80);
  MEMCHR(RES, z6, a, offset, c, n); // no alarm
  //@assert (z6 >= -1 && z6 <= 79);
  //@assert refined: (z6 == -1 || z6 == 75);

  init_array_nondet(a, 0, 99, 0, 2);
  offset = RANGE(50, 70);
  MEMCHR(RES, z7, a, offset, c, n);
  //@assert (z7 >= -1 && z7 <= 89);

  n = RANGE(0, 100);
  MEMCHR(RES, z8, a, offset, c, n); // alarm: Invalid
  //@assert (z8 >= -1 && z8 <= 99);

  offset = RANGE(0, 10);
  n = RANGE(0, 90);
  MEMCHR(RES, z9, a, offset, c, n); // no alarm
  //@assert (z9 >= -1 && z9 <= 99);
}

void memchr_unbounded_n() {
  CHAR c = 0;
  int n = nondet;
  if (n < 0) n = 0;
  CHAR_PTR(s);
  STRING(s,"abc");
  MEMCHR(RES, zu1, s, 0, c, n); // warning from precondition
  //@ assert (zu1 == -1 || zu1 == 3);
  Ival o = NONDET(0,1);
  MEMCHR(RES, zu2, s, o, c, n); // warning from precondition
  //@ assert (zu2 == -1 || zu2 == 2 || zu2 == 3);
  STRING(s,"bcd\0eg");
  MEMCHR(RES, zu3, s, 0, c, n); // warning from precondition
  //@ assert (zu3 == -1 || zu3 == 3);
}

//DELPHIC_TEST
void memchr_intervals() {
  CHAR c = 0;
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 9, 0, 1);
  init_array_nondet(a, 3, 6, 1, 1);
  init_array_nondet(a, 11, 11, 0, 1);
  init_array_nondet(a, 12, 15, 1, 1);
  init_array_nondet(a, 16, 19, 0, 1);

  Ival offset = RANGE(0,9);
  Ival n = RANGE(0, 10);
  MEMCHR(RES, z1, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z1 >= -1 && z1 <= 9);
  //@ assert refined: (z1 == -1 || z1 == 0 || z1 == 1 || z1 == 2 || z1 == 7 || z1 == 8 || z1 == 9);

  offset = RANGE(3,9);
  n = RANGE(2,10);
  MEMCHR(RES, z2, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z2 == -1 || z2 == 7 || z2 == 8 || z2 == 9);

  offset = RANGE(3,9);
  n = RANGE(0,11);
  MEMCHR(RES, z3, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z3 == -1 || z3 == 7 || z3 == 8 || z3 == 9);

  offset = RANGE(3,10);
  n = RANGE(0,9);
  MEMCHR(RES, z4, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z4 == -1 || z4 == 7 || z4 == 8 || z4 == 9);

  offset = RANGE(3,10);
  n = RANGE(0,10);
  MEMCHR(RES, z5, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z5 == -1 || z5 == 7 || z5 == 8 || z5 == 9);

  offset = RANGE(3,10);
  n = RANGE(0,11);
  MEMCHR(RES, z6, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z6 == -1 || z6 == 7 || z6 == 8 || z6 == 9);

  offset = RANGE(3,11);
  n = RANGE(0,10);
  MEMCHR(RES, z7, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z7 >= -1 && z7 <= 19);

  offset = RANGE(3,11);
  n = RANGE(0,10);
  MEMCHR(RES, z8, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z8 >= -1 && z8 <= 19);

  offset = RANGE(3,11);
  n = RANGE(0,11);
  MEMCHR(RES, z9, a, offset, c, n); // warning from precondition (a[10] UNINIT)
  //@ assert (z9 >= -1 && z9 <= 19);
}

// In the macro below: alarm from precondition in first call to memchr
#define SMALL_SETS_NO_ASSERTIONS(i, c, res)     \
  CHAR_ARRAY(t_ ## i, 4);                       \
  do {                                          \
    CHAR_PTR(s);                                \
    STRING(s,"abc");                            \
    Ival o = NONDET(0, 1);                      \
    MEMCHR_no_decl(res[0], s, o, c, 4);         \
    STRING(s,"\0bc");                           \
    o = NONDET(0,1);                            \
    MEMCHR_no_decl(res[1], s, o, c, 3);         \
    STRING(s,"");                               \
    MEMCHR_no_decl(res[2], s, 0, c, 0);         \
    STRING(s,"b\0c");                           \
    o = NONDET(0,2);                            \
    MEMCHR_no_decl(res[3], s, o, c, 2);         \
    t_ ## i[0] = t_ ## i[1] = NONDET(c, 1);     \
    t_ ## i[2] = t_ ## i[3] = 1;                \
    MEMCHR_no_decl(res[4], t_ ## i, 0, c, 4);   \
  } while (0)

//DELPHIC_TEST
void memchr_small_sets_chars() {
  // NOTE: some tests here may contain extra results due to imprecisions. In such cases, an extra test containing
  // a more imprecise range is performed before it.
  // This means that only the second test is allowed to return "unknown".
  CHAR c = 'a';
  RES res[5];
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(0, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == 0); // alarm
  //@ assert (res[1] == -1); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] == -1); // no alarm
  //@ assert (res[4] == -1 || res[4] == 0 || res[4] == 1); // no alarm

  c = 'b';
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(1, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == 1); // no alarm
  //@ assert (res[1] == 1); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] == -1 || res[3] == 0); // no alarm
  //@ assert (res[4] == -1 || res[4] == 0 || res[4] == 1); // no alarm

  c = NONDET('a', 'b'); // c IN {a; b}
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(2, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == -1 || res[0] == 0 || res[0] == 1); // alarm
  //@ assert refined: (res[0] == 0 || res[0] == 1); // alarm
  //@ assert (res[1] == -1 || res[1] == 1); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] == -1 || res[3] == 0); // no alarm
  //@ assert (res[4] == -1 || res[4] == 0 || res[4] == 1); // no alarm

  c = NONDET('b', 'c'); // c IN {b; c}
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(3, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == -1 || res[0] == 1 || res[0] == 2); // no alarm
  //@ assert refined: (res[0] == 1 || res[0] == 2); // no alarm
  //@ assert (res[1] == -1 || res[1] == 1 || res[1] == 2); // no alarm
  //@ assert refined: (res[1] == 1 || res[1] == 2); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] == -1 || res[3] == 0 || res[3] == 2); // no alarm
  //@ assert (res[4] == -1 || res[4] == 0 || res[4] == 1); // no alarm

  IF_NONDET(c, 0); // c IN {b; c; 0}
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(4, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == -1 || res[0] >= 1 && res[0] <= 3); // no alarm
  //@ assert refined: (res[0] >= 1 && res[0] <= 3); // no alarm
  //@ assert (res[1] >= -1 && res[1] <= 3); // no alarm
  //@ assert refined: (res[1] >= 0 && res[1] <= 3); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] >= -1 && res[3] <= 3); // no alarm
  //@ assert (res[4] == -1 || res[4] == 0 || res[4] == 1); // no alarm
}

int main () {
  memchr_small_sets();
  memchr_zero_termination();
  memchr_initialization();
  memchr_large();
  memchr_large_uninit();
  memchr_misc_array();
  memchr_misc();
  memchr_misc2();
  memchr_bitfields();
  memchr_bitfields2();
  memchr_escaping();
  memchr_big_array();
  memchr_no_zero_but_ok();
  memchr_small_sets_n();
  memchr_large_n();
  memchr_unbounded_n();
  memchr_intervals();
  memchr_small_sets_chars();

  return 0;
}
