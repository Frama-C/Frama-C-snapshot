#include "__fc_builtin.h"
#include "string.h"
#include <wchar.h>
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
#define CHAR wchar_t
#define NONDET(a,b) (nondet ? (a) : (b))
#define NONDET_STR(a,b) NONDET((a),(b))
#define NONDET_STR2(a,b) NONDET((a),(b))
#define IF_NONDET(var,val) if (nondet) var = val
#define RANGE(from,to) Frama_C_interval(from,to)
#define CHAR_ARRAY(var,n) wchar_t var[n]
#define CHAR_PTR(var) wchar_t *var
#define STRING(var,str) var = str;

const wchar_t* static_str = L"Hello World\n";
const wchar_t* zero_str = L"abc\0\0\0abc";

#define TSZ 12
const wchar_t* tab_str[TSZ] =
  {
    L"" , // 0
    L"a", // 1
    L"aa" , // 2
    L"aaa" , // 3
    L"aaaa" , // 4
    L"aaaaa" , // 5
    L"aaaaaa" , // 6
    /* hole */
    L"aaaaaaaaa" , // 9
    L"aaaaaaaaaa" ,
    L"aaaaaaaaaaa",
    L"aaaaaaaaaaaa" ,
    L"aaaaaaaaaaaaa" }; // 13

wchar_t unterminated_string[12] = L"unterminated";

#define WCSLEN(s, offs) wcslen(s+offs)



void small_sets() {
  wchar_t *s = L"abc";
  wchar_t *p = nondet ? s : (s+1);
  int z1 = wcslen(p);
  //@ assert (z1 == 2 || z1 == 3);

  s = L"\0bc";
  p = nondet ? s : (s+1);
  int z2 = wcslen(p);
  //@ assert (z2 == 0 || z2 == 2);

  s = L"";
  p = s;
  int z3 = wcslen(p);
  //@ assert (z3 == 0);

  s = L"b\0c";
  p = nondet ? s : (s+2);
  int z4 = wcslen(p);
  //@ assert (z4 == 1);

  wchar_t t[4];
  t[0] = t[1] = nondet ? 0 : 1;
  t[2] = t[3] = 1;
  int z5 = wcslen(t); // warning
  //@ assert z5 == 0 || z5 == 1;

}

void zero_termination() {
  wchar_t empty_or_non_terminated[1];
  empty_or_non_terminated[0] = nondet ? 0 : 100;
  int z1 = wcslen(empty_or_non_terminated);
  //@ assert (z1 == 0);

  wchar_t non_terminated[1] = {'X'};
  assert_bottom(wcslen(non_terminated), non_terminated);

  wchar_t non_terminated2[4] = { 76, 0, 28, 14 };
  wchar_t *p = nondet ? ((wchar_t *)&non_terminated2) + 2 : ((wchar_t *)&non_terminated2) + 3;
  assert_bottom(wcslen(p), p);

}

//DELPHIC_TEST
void wcslen_initialization() {
  CHAR_ARRAY(empty_or_uninitialized, 1);
  IF_NONDET(empty_or_uninitialized[0], 0);
  RES z1 = WCSLEN(empty_or_uninitialized, 0); // alarm
  //@ assert (z1 == 0);

  CHAR_ARRAY(uninitialized, 1);
  assert_bottom(WCSLEN(uninitialized, 0), uninitialized);

  CHAR_ARRAY(s, 2);
  IF_NONDET(s[0], 1);
  s[1] = 0;
  CHAR_PTR(p);
  p = s;
  RES z2 = WCSLEN(p, 0); // alarm
  //@ assert (z2 == 1);

  CHAR_ARRAY(t, 4);
  t[0] = t[1] = 10;
  IF_NONDET(t[2], 10);
  t[3] = 0;
  p = t;
  RES z3 = WCSLEN(p, 0); // alarm
  //@ assert (z3 == 3);
}

typedef struct {
  int64_t a:32;
  int64_t b:32;
  int64_t c:63;
} st;

void bitfields() {
  st s;
  s.a = 3;
  s.b = 1;
  s.c = 7;
  wchar_t *p = &s;
  assert_bottom(wcslen(p), p);
}

typedef struct {
  int64_t a:33;
  int64_t b:63;
  int64_t c:63;
} st2;

void bitfields2() {
  st2 s;
  s.a = 3;
  s.b = 1;
  s.c = 0x70000;
  wchar_t *p = &s;
  int z1 = wcslen(p);
  //@assert (z1 == 2);
}

void init_array_nondet(wchar_t *a, int from, int to, int val1, int val2) {
  int val = NONDET(val1, val2);
  memset(a + from, val, sizeof(wchar_t)*(to-from+1));
  from = to = val1 = val2 = -1; // reset to minimize oracle changes
}

//DELPHIC_TEST
void wcslen_large() {
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 99, 1, 2);
  a[20] = 0;
  a[75] = 0;
  Ival offset = RANGE(3, 30);
  RES z1 = WCSLEN(a, offset);
  //@ assert (z1 >= 0 && z1 <= 54);

  offset = RANGE(5, 17);
  RES z2 = WCSLEN(a, offset);
  //@ assert (z2 >= 3 && z2 <= 15);

  offset = RANGE(60, 74);
  RES z3 = WCSLEN(a, offset);
  //@ assert (z3 >= 1 && z3 <= 15);

  offset = RANGE(63, 80);
  RES z4 = WCSLEN(a, offset); // alarm
  //@ assert (z4 >= 0 && z4 <= 12);

  init_array_nondet(a, 0, 99, 0, 2);
  offset = RANGE(50, 70);
  RES z5 = WCSLEN(a, offset); // alarm
  //@ assert (z5 >= 0 && z5 <= 49);

}

//DELPHIC_TEST
void wcslen_large_uninit() {
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 39, 1, 2);
  init_array_nondet(a, 50, 94, 3, 4);
  a[20] = 0;
  a[75] = 0;

  Ival offset = RANGE(3, 30);
  RES z1 = WCSLEN(a, offset); // alarm
  //@ assert (z1 >= 0 && z1 <= 17);

  a[98] = 0;
  offset = RANGE(63, 80);
  RES z2 = WCSLEN(a, offset); // alarm
  //@ assert (z2 >= 0 && z2 <= 12);

  offset = RANGE(45, 55);
  RES z3 = WCSLEN(a, offset); // alarm
  //@ assert (z3 >= 20 && z3 <= 25);

  offset = 0; // avoid oracle diffs when changed
}

void escaping() {
  wchar_t s[4];
  {
    int x;
    *((int *)s) = (int)&x; *((int *)&s[1]) = (int)&x; *((int *)&s[2]) = (int)&x; *((int *)&s[3]) = (int)&x;
  }
  if (nondet) s[0] = 0;
  int z1 = wcslen(s); // alarm
  //@ assert (z1 == 0);
  s[0] = 0;
  int z2 = wcslen(s); // no alarm
  //@ assert (z2 == 0);
}

void misc() {
  const wchar_t* loc_str = L"Bonjour Monde\n";
  wchar_t loc_char_array[5];
  size_t sz1,sz2,sz3,sz4,sz5,sz6,sz7,sz8;
  int x[4]; x[0] = 0; x[1] = 0xef; x[2] = 0xcd; x[3] = 0xab;
  int z[4]; z[0] = 0; z[1] = 0x56; z[2] = 0x23; z[3] = 0x12;
  int i;
  wchar_t *str;

  assert_bottom(wcslen(unterminated_string), unterminated_string);

  str = nondet ? static_str : loc_str;
  sz1 = wcslen(str);
  //@ assert(sz1 == 12) || (sz1 == 14);
  str = x;
  str = nondet ? str : str + 3;
  sz2 = wcslen(str);
  //@ assert(sz2 == 0) ; // no, could also do an RTE
  i = Frama_C_interval(0,TSZ-1);
  str = tab_str[i];
  sz3 = wcslen(str);
  //@ assert (sz3 >= 0) && (sz3 <= 13);
  loc_char_array[3] = L'\0';
  assert_bottom(wcslen(loc_char_array), loc_char_array);
  sz4 = wcslen(zero_str);
  //@ assert(sz4 == 3);
  wchar_t *s1 = nondet ? L"abc" : L"ABCD";
  wchar_t *s2 = nondet ? s1 : s1+1;
  sz5 = wcslen(s2);
  //@ assert(sz5 >= 2 && sz5 <= 4);

  s1 = nondet ? L"efg" : L"EFGH";
  s2 = nondet ? s1+1 : s1+2;
  sz6 = wcslen(s2);
  //@ assert(sz6 >= 1 && sz6 <= 3);

  s1 = nondet ? L"mno\0pqr" : L"MNOP\0QRS";
  for (int j = 0; j < 8; j++) {
    sz7 = wcslen(s1 + j);
    //@ assert(sz7 >= 0 && sz7 <= 4);
  }

  wchar_t maybe_init[2];
  maybe_init[1] = L'\0';
  if (nondet) maybe_init[0] = L'A';
  sz8 = wcslen(maybe_init);
  //@ assert(sz8 == 1);
}

void big_array () {
  int64_t t[1000000];
  int64_t u[200];
  int64_t r[201];
  int64_t *p;
  p = &t[nondet];
  *p = 0x1023678900000000;
  //int64_t v[2] = {0x00006767, 0x10102323};
  //memcpy(p, v, sizeof(v));// *p = 0x67676767; *(p+1) = 0; *(p+2) = 0x23232323; *(p+3) = 0x10101010;
  p = &u[nondet];
  *p = 0x1023678900000000;
  //memcpy(p, v, sizeof(v));// *p = 0x67676767; *(p+1) = 0; *(p+2) = 0x23232323; *(p+3) = 0x10101010;
  p = &r[nondet];
  *p = 0x1023678900000000;
  //memcpy(p, v, sizeof(v));// *p = 0x67676767; *(p+1) = 0; *(p+2) = 0x23232323; *(p+3) = 0x10101010;

  unsigned long len_u;
  unsigned long len_r;
  unsigned long len_t;

  len_u = wcslen((wchar_t const *)u); // below plevel; precise
  len_r = wcslen((wchar_t const *)r); // above plevel; imprecise
  len_t = wcslen((wchar_t const *)t); // *far* above plevel
  Frama_C_show_each(len_u, len_r, len_t);

  len_u = wcslen((wchar_t const *)(u+nondet)); // should be precise
  len_r = wcslen((wchar_t const *)(r+nondet));
  len_t = wcslen((wchar_t const *)(t+nondet));
  Frama_C_show_each(len_u, len_r, len_t);
}

void negative_offsets() {
  wchar_t buf[100];
  for (int i = 0; i < 100; i++) buf[i] = L'A'; //avoid memset due to C++ oracles
  buf[99] = 0;
  unsigned len1, len2, len3, len4, len5, len6;
  if (nondet) {
    int offset1 = Frama_C_interval(-10, -8);
    len1 = wcslen(buf + offset1);
  }
  if (nondet) {
    int offset2 = Frama_C_interval(-2, -1);
    len2 = wcslen(buf + offset2);
  }
  int offset3 = Frama_C_interval(-1, -0);
  len3 = wcslen(buf + offset3);
  int offset4 = Frama_C_interval(-1, 2);
  len4 = wcslen(buf + offset4);
  int offset5 = Frama_C_interval(-4, 7);
  len5 = wcslen(buf + offset5);
  int offset6 = Frama_C_interval(-10, 0);
  wchar_t *p = buf + offset6;
  len6 = wcslen(p);
  *(p+len6) = 0;
  wchar_t dest[100 * 2];
}

int main (int c) {
  small_sets();
  zero_termination();
  wcslen_initialization();
  wcslen_large();
  wcslen_large_uninit();
  misc();
  bitfields();
  bitfields2();
  escaping();
  big_array();
  negative_offsets();
  return 0;
}
