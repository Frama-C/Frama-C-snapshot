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

#define STRCHR_no_decl(lv_var, _s, _offs, _c)   \
  do {                                          \
    char *_ss = strchr(_s+_offs,_c);            \
    int _res;                                   \
    if (_ss == 0) {                             \
      _res = -1;                                \
    } else {                                    \
      _res = _ss - _s;                          \
    }                                           \
    Frama_C_show_each_mystrchr(_res);           \
    lv_var = _res;                              \
  } while (0)                                   \

#define STRCHR(lv_type, lv_var, _s, _offs, _c)  \
  lv_type lv_var;                               \
  STRCHR_no_decl(lv_var, _s, _offs, _c)         \

#define STRCHR_bottom(s, offs, c) assert_bottom(strchr(s+offs,c), s)

#define STRCHR2(lv_type, lv_var, _base, _offs1, _offs2, _c)          \
  lv_type lv_var;                                                       \
  do {                                                                  \
    const char *_p = ((const char *)_base)+NONDET(_offs1, _offs2);      \
    char *_s = strchr(_p,_c);                                           \
    int _res;                                                           \
    if (_s == 0) {                                                      \
      _res = -1;                                                        \
    } else {                                                            \
      _res = _s - (char*)_base;                                         \
    }                                                                   \
    Frama_C_show_each_mystrchr2(_res);                                  \
    lv_var = _res;                                                      \
  } while (0)                                                           \

//DELPHIC_TEST
void strchr_small_sets() {
  CHAR c = 0;
  CHAR_PTR(s);
  STRING(s,"abc");
  Ival o = NONDET(0, 1);
  STRCHR(RES, z1, s, o, c);
  //@ assert (z1 == 3);
  STRING(s,"\0bc");
  o = NONDET(0,1);
  STRCHR(RES, z2, s, o, c);
  //@ assert (z2 == 0 || z2 == 3);
  STRING(s,"");
  STRCHR(RES, z3, s, 0, c);
  //@ assert (z3 == 0);
  STRING(s,"b\0c");
  o = NONDET(0,2);
  STRCHR(RES, z4, s, o, c);
  //@ assert (z4 == 1 || z4 == 3);
  CHAR_ARRAY(t, 4);
  t[0] = t[1] = NONDET(0, 1);
  t[2] = t[3] = 1;
  STRCHR(RES, z5, t, 0, c); // warning
  //@ assert (z5 == -1 || z5 == 0 || z5 == 1);
}

//DELPHIC_TEST
void strchr_zero_termination() {
  CHAR c = 0;
  CHAR_ARRAY(empty_or_non_terminated, 1);
  empty_or_non_terminated[0] = NONDET(0, 100);
  STRCHR(RES, z1, empty_or_non_terminated, 0, c); // alarm
  //@ assert (z1 == -1 || z1 == 0);

  CHAR_ARRAY(non_terminated, 1);
  non_terminated[0] = 'X';
  STRCHR_bottom(non_terminated, 0, c);

  CHAR_ARRAY(non_terminated2, 4);
  non_terminated2[0] = 76; non_terminated2[1] = 0;
  non_terminated2[2] = 28; non_terminated2[3] = 14;
  Ival o = NONDET(2, 3);
  STRCHR_bottom(non_terminated2, 2, c);
}

//DELPHIC_TEST
void strchr_initialization() {
  CHAR c = 0;
  CHAR_ARRAY(empty_or_uninitialized, 1);
  IF_NONDET(empty_or_uninitialized[0], 0);
  STRCHR(RES, z1, empty_or_uninitialized, 0, c); // alarm
  //@ assert (z1 == 0);

  CHAR_ARRAY(uninitialized, 1);
  STRCHR_bottom(uninitialized, 0, c);

  CHAR_ARRAY(s, 2);
  IF_NONDET(s[0], 1);
  s[1] = 0;
  STRCHR(RES, z2, s, 0, c); // alarm
  //@ assert (z2 == 1);

  CHAR_ARRAY(t,4);
  t[0] = t[1] = 10;
  IF_NONDET(t[2], 10);
  t[3] = 0;
  STRCHR(RES, z3, t, 0, c); // alarm
  //@ assert (z3 == 3);
}

typedef struct {
  int a:8;
  int b:8;
  int c:17;
} st;

void strchr_bitfields() {
  CHAR c = 0;
  st s;
  s.a = 3;
  s.b = 1;
  s.c = 7;
  CHAR_PTR(p);
  p = &s;
  assert_bottom(strchr(p, c),bitfields);
}

typedef struct {
  int a:4;
  int b:4;
  int c:17;
} st2;

void strchr_bitfields2() {
  CHAR c = 0;
  st2 s;
  s.a = 3;
  s.b = 1;
  s.c = 7;
  CHAR_PTR(p);
  p = &s;
  STRCHR(RES, z1, p, 0, c);
  //@assert (z1 == 2);
}

void init_array_nondet(void *a, int from, int to, int val1, int val2) {
  int val = NONDET(val1, val2);
  memset(((char*)a) + from, val, to-from+1);
  from = to = val1 = val2 = -1; // reset to minimize oracle changes
}

//DELPHIC_TEST
void strchr_large() {
  CHAR c = 0;
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 99, 1, 2);
  a[20] = 0;
  a[75] = 0;
  Ival offset = RANGE(3, 30);
  STRCHR(RES, z1, a, offset, c);
  //@ assert (z1 == 20 || z1 == 75);

  offset = RANGE(5, 17);
  STRCHR(RES, z2, a, offset, c);
  //@ assert (z2 >= -1 && z2 <= 20);
  //@ assert refined: (z2 == 20);

  offset = RANGE(60, 74);
  STRCHR(RES, z3, a, offset, c);
  //@ assert (z3 >= -1 && z3 <= 75);
  //@ assert refined: (z3 == 75);

  offset = RANGE(63, 80);
  STRCHR(RES, z4, a, offset, c); // alarm
  //@ assert (z4 >= -1 && z4 <= 79);
  //@ assert refined: (z4 == 75);

  init_array_nondet(a, 0, 99, 0, 2);
  offset = RANGE(50, 70);
  STRCHR(RES, z5, a, offset, c); // alarm
  //@ assert (z5 >= -1 && z5 <= 99);
}

//DELPHIC_TEST
void strchr_large_uninit() {
  CHAR c = 0;
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 39, 1, 2);
  init_array_nondet(a, 50, 94, 3, 4);
  a[20] = 0;
  a[75] = 0;

  Ival offset = RANGE(3, 30);
  STRCHR(RES, z1, a, offset, c); // alarm: Uninit
  //@ assert (z1 >= -1 && z1 <= 29);
  //@ assert refined: (z1 == 20);

  a[98] = 0;
  offset = RANGE(63, 80);
  STRCHR(RES, z2, a, offset, c); // alarm: Uninit
  //@ assert (z2 >= -1 && z2 <= 79);
  //@ assert refined: (z2 == 75);

  offset = RANGE(45, 55);
  STRCHR(RES, z3, a, offset, c); // alarm: Uninit
  //@ assert (z3 >= -1 && z3 <= 75);
  //@ assert refined: (z3 == 75);

  offset = 0; // avoid oracle diffs when changed
}

void strchr_escaping() {
  CHAR c = 0;
  CHAR_ARRAY(s,4);
  {
    int x;
    *((int *)s) = (int) &x;
  }
  IF_NONDET(s[0], 0);
  STRCHR(RES, z1, s, 0, c); // alarm
  //@ assert (z1 == 0);
  s[0] = 0;
  STRCHR(RES, z2, s, 0, c); // no alarm
  //@ assert (z2 == 0);
}

void strchr_misc_array() {
  CHAR c = 0;
  Ival i = RANGE(0,TSZ-1);
  STRCHR(RES, sz3, tab_str[i], 0, c);
  //@ assert (sz3 >= 0 && sz3 <= 13);
}

//DELPHIC_TEST
void strchr_misc() {
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

  STRCHR_bottom(unterminated_string, 0, c);

  str = NONDET_STR2(static_str, loc_str);
  STRCHR(RES, sz1, str, 0, c); // false alarm (pointers from 2 bases)
  //@ assert(sz1 == 12 || sz1 == 14);
  str = &x;
  STRCHR2(RES, sz2, str, 0, 3, c);
  //@ assert(sz2 == 0); // alarm
  loc_char_array[3] = '\0';
  STRCHR_bottom(loc_char_array, 0, c);
  STRCHR(RES, sz4a, zero_str, 0, c);
  //@ assert(sz4a == 3);
  STRCHR(RES, sz4b, zero_str, 4, c);
  //@ assert(sz4b == 4);
  STRCHR(RES, sz4c, zero_str, 5, c);
  //@ assert(sz4c == 5);
  STRCHR(RES, sz4d, zero_str, 6, c);
  //@ assert(sz4d == 9);
}

//DELPHIC_TEST
void strchr_misc2() {
  CHAR c = 0;
  CHAR_PTR(s1);
  s1 = NONDET_STR("abc", "ABCD");
  STRCHR2(RES, sz5, s1, 0, 1, c);
  //@ assert (sz5 == 3 || sz5 == 4);

  s1 = NONDET_STR("efg", "EFGH");
  STRCHR2(RES, sz6, s1, 1, 2, c);
  //@ assert (sz6 == 3 || sz6 == 4);

  s1 = NONDET_STR("mno\0pqr", "MNOP\0QRS");
  for (int j = 0; j < 8; j++) {
    STRCHR(RES, sz7, s1, j, c);
    //@ assert (sz7 == 3 || sz7 == 4 || sz7 == 7 || sz7 == 8);
  }

  CHAR_ARRAY(maybe_init, 2);
  maybe_init[1] = '\0';
  IF_NONDET(maybe_init[0], 'A');
  STRCHR(RES, sz8, maybe_init, 0, c); // alarm
  //@ assert(sz8 == 1);
}

void strchr_big_array () {
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

  // the actual length is unsigned, but due to our representation
  // of NULL via -1, we actually use it as signed
  long len_u;
  long len_r;
  long len_t;
  // All strchr calls in this function emit alarms due to preconditions.
  // without initialization, most accesses are invalid, so the result is precise
  STRCHR_no_decl(len_u, (char const *)u, 0, c); // below plevel; precise
  STRCHR_no_decl(len_r, (char const *)r, 0, c); // above plevel; imprecise
  STRCHR_no_decl(len_t, (char const *)t, 0, c); // *far* above plevel
  Frama_C_show_each(len_u, len_r, len_t);
  //@ assert (len_u == 1);
  //@ assert (len_r == 1);
  //@ assert (len_t == 1);

  STRCHR_no_decl(len_u, (char const *)u, nondet, c); // should be precise
  STRCHR_no_decl(len_r, (char const *)r, nondet, c);
  STRCHR_no_decl(len_t, (char const *)t, nondet, c);
  Frama_C_show_each(len_u, len_r, len_t);
  //@ assert (len_u >= -1 && len_u <= 799);
  //@ assert (len_r >= -1 && len_r <= 803);
  //@ assert (len_t >= -1 && len_t <= 3999999);
  //@ assert refined: (len_u >= 0 && len_u <= 3);
  //@ assert refined: (len_r >= 0 && len_r <= 800);
  //@ assert refined: (len_t >= 0 && len_t <= 3999996);

  init_array_nondet(u, 0, sizeof(int)*200-1, 0, 1);
  init_array_nondet(r, 0, sizeof(int)*201-1, 0, 1);
  init_array_nondet(t, 0, sizeof(int)*1000000-1, 0, 1);
  p = &u[nondet];
  *p = 0x10230067;
  p = &r[nondet];
  *p = 0x10230067;
  p = &t[nondet];
  *p = 0x10230067;
  STRCHR_no_decl(len_u, (char const *)u, 0, c); // below plevel; precise
  STRCHR_no_decl(len_r, (char const *)r, 0, c); // above plevel; imprecise
  STRCHR_no_decl(len_t, (char const *)t, 0, c); // *far* above plevel
  Frama_C_show_each(len_u, len_r, len_t);
  //@ assert (len_u >= 0 && len_u <= 799);
  //@ assert (len_r >= 0 && len_r <= 803);
  //@ assert (len_t >= 0 && len_t <= 3999999);

  STRCHR_no_decl(len_u, (char const *)u, nondet, c); // should be precise
  STRCHR_no_decl(len_r, (char const *)r, nondet, c);
  STRCHR_no_decl(len_t, (char const *)t, nondet, c);
  Frama_C_show_each(len_u, len_r, len_t);
  //@ assert (len_u >= -1 && len_u <= 799);
  //@ assert (len_r >= -1 && len_r <= 803);
  //@ assert (len_t >= -1 && len_t <= 3999999);
  //@ assert refined: (len_u >= 0 && len_u <= 799);
  //@ assert refined: (len_r >= 0 && len_r <= 803);
  //@ assert refined: (len_t >= 0 && len_t <= 3999999);
}

//DELPHIC_TEST
void strchr_no_zero_but_ok() {
  CHAR c = 0;
  CHAR_ARRAY(s,20);
  s[0] = s[1] = s[2] = s[3] = s[4] = s[5] = s[6] = s[7] = s[8] = s[9] = 1;
  s[10] = 0;
  s[11] = s[12] = s[13] = 1;
  s[14] = s[15] = s[16] = s[17] = s[18] = s[19] = NONDET(1, 0);
  STRCHR(RES, z1, s, 0, c);
  //@assert (z1 == 10);
  STRCHR(RES, z2, s, 0, c);
  //@assert (z2 == 10);
  Ival offs = NONDET(1, 8);
  STRCHR(RES, z3, s, offs, c);
  //@assert (z3 == 10);
  offs = NONDET(7, 11);
  STRCHR(RES, z4, s, offs, c); // alarm
  //@assert (z4 >= -1 && z4 <= 19);
  offs = NONDET(7, 18);
  STRCHR(RES, z5, s, offs, c); // alarm
  //@assert (z5 == -1 || z5 == 10 || z5 == 18 || z5 == 19);
}

// In the macro below: alarm from precondition in first call to strchr
#define SMALL_SETS_NO_ASSERTIONS(i, c, res)     \
  CHAR_ARRAY(t_ ## i, 4);                       \
  do {                                          \
    CHAR_PTR(s);                                \
    STRING(s,"abc");                            \
    Ival o = NONDET(0, 1);                      \
    STRCHR_no_decl(res[0], s, o, c);            \
    STRING(s,"\0bc");                           \
    o = NONDET(0,1);                            \
    STRCHR_no_decl(res[1], s, o, c);            \
    STRING(s,"");                               \
    STRCHR_no_decl(res[2], s, 0, c);            \
    STRING(s,"b\0c");                           \
    o = NONDET(0,2);                            \
    STRCHR_no_decl(res[3], s, o, c);            \
    t_ ## i[0] = t_ ## i[1] = NONDET(c, 1);     \
    t_ ## i[2] = 1;                             \
    t_ ## i[3] = 0;                             \
    STRCHR_no_decl(res[4], t_ ## i, 0, c);      \
  } while (0)

//DELPHIC_TEST
void strchr_small_sets_chars() {
  CHAR c = 'a';
  RES res[5];
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(0, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == -1 || res[0] == 0); // no alarm
  //@ assert (res[1] == -1); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] == -1); // no alarm
  //@ assert (res[4] >= -1 && res[4] <= 1); // no alarm

  c = 'b';
  SMALL_SETS_NO_ASSERTIONS(1, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == 1); // no alarm
  //@ assert (res[1] == -1 || res[1] == 1); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] == -1 || res[3] == 0); // no alarm
  //@ assert (res[4] >= -1 && res[4] <= 1); // no alarm

  c = NONDET('a', 'b'); // c IN {'a'; 'b'}
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(2, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  // for the first assert below, we may be looking for 'a' starting from s+1
  //@ assert (res[0] == -1 || res[0] == 0 || res[0] == 1); // no alarm
  //@ assert (res[1] == -1 || res[0] == 1 || res[1] == 1); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] == -1 || res[3] == 0); // no alarm
  //@ assert (res[4] >= -1 && res[4] <= 1); // no alarm

  c = NONDET('b', 'c'); // c IN {'b'; 'c'}
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(3, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == -1 || res[0] == 1 || res[0] == 2); // no alarm
  //@ assert refined: (res[0] == 1 || res[0] == 2); // no alarm
  //@ assert (res[1] == -1 || res[1] == 1 || res[1] == 2); // no alarm
  //@ assert (res[2] == -1); // no alarm
  //@ assert (res[3] == -1 || res[3] == 0 || res[3] == 2); // no alarm
  //@ assert (res[4] >= -1 && res[4] <= 1); // no alarm

  IF_NONDET(c, 0); // c IN {0; 'b'; 'c'}
  Frama_C_show_each_c(c);
  SMALL_SETS_NO_ASSERTIONS(4, c, res);
  Frama_C_show_each_res(res[0], res[1], res[2], res[3], res[4]);
  //@ assert (res[0] == -1 || res[0] >= 1 && res[0] <= 3); // no alarm
  //@ assert refined: (res[0] >= 1 && res[0] <= 3); // no alarm
  //@ assert (res[1] >= -1 && res[1] <= 3); // no alarm
  //@ assert (res[2] == -1 || res[2] == 0); // no alarm
  //@ assert (res[3] >= -1 && res[3] <= 3); // no alarm
  //@ assert (res[4] == -1 || res[4] == 0 || res[4] == 1 || res[4] == 3); // no alarm
}

void strchr_unbounded() {
  CHAR c = nondet;
  CHAR_PTR(s);
  STRING(s,"abcd");
  STRCHR(RES, u1, s, 0, c);
  //@ assert (u1 >= -1 && u1 <= 4); // no alarm
  STRING(s,"ABCDEFGHIJKLMNOPQRSTUVWXYZ");
  CHAR_ARRAY(t,30); // uninitialized
  IF_NONDET(s, t);
  STRCHR(RES, u2, s, 0, c); // alarm
  //@ assert (u2 >= -1 && u2 <= 26); // alarm
  init_array_nondet(t, 0, 29, 0, 1);
  STRCHR(RES, u3, s, 0, c); // alarm
  //@ assert (u3 >= -1 && u3 <= 29); // alarm
  STRCHR(RES, u4, s, nondet, c); // alarm
  //@ assert (u4 >= -1 && u4 <= 29); // alarm
}

void strchr_invalid() {
  CHAR_PTR(s);
  STRING(s,"hello");
  STRCHR(RES, unused, s, (unsigned int)&s, 1); // alarm
}

void strchr_garbled_mix_in_char() {
  int x;
  char *garbled = ((int)(&x + (int)&x));
  if (nondet) strchr(garbled, (int)garbled); // must not crash
}

int main () {
  strchr_small_sets();
  strchr_zero_termination();
  strchr_initialization();
  strchr_large();
  strchr_large_uninit();
  strchr_misc_array();
  strchr_misc();
  strchr_misc2();
  strchr_bitfields();
  strchr_bitfields2();
  strchr_escaping();
  strchr_big_array();
  strchr_no_zero_but_ok();
  strchr_small_sets_chars();
  strchr_unbounded();
  strchr_invalid();
  strchr_garbled_mix_in_char();

  return 0;
}
