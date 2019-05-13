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

const char* static_str = "Hello World\n";
const char* zero_str = "abc\0\0\0abc";

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

char unterminated_string[12] = "unterminated";

#define STRLEN(s, offs) strlen(s+offs)



void small_sets() {
  char *s = "abc";
  char *p = nondet ? s : (s+1);
  int z1 = strlen(p);
  //@ assert (z1 == 2 || z1 == 3);

  s = "\0bc";
  p = nondet ? s : (s+1);
  int z2 = strlen(p);
  //@ assert (z2 == 0 || z2 == 2);

  s = "";
  p = s;
  int z3 = strlen(p);
  //@ assert (z3 == 0);

  s = "b\0c";
  p = nondet ? s : (s+2);
  int z4 = strlen(p);
  //@ assert (z4 == 1);

  char t[4];
  t[0] = t[1] = nondet ? 0 : 1;
  t[2] = t[3] = 1;
  int z5 = strlen(t); // warning
  //@ assert z5 == 0 || z5 == 1;

}

void zero_termination() {
  char empty_or_non_terminated[1];
  empty_or_non_terminated[0] = nondet ? 0 : 100;
  int z1 = strlen(empty_or_non_terminated);
  //@ assert (z1 == 0);

  char non_terminated[1] = {'X'};
  assert_bottom(strlen(non_terminated), non_terminated);

  char non_terminated2[4] = { 76, 0, 28, 14 };
  char *p = nondet ? ((char *)&non_terminated2) + 2 : ((char *)&non_terminated2) + 3;
  assert_bottom(strlen(p), p);

}

//DELPHIC_TEST
void strlen_initialization() {
  CHAR_ARRAY(empty_or_uninitialized, 1);
  IF_NONDET(empty_or_uninitialized[0], 0);
  RES z1 = STRLEN(empty_or_uninitialized, 0); // alarm
  //@ assert (z1 == 0);

  CHAR_ARRAY(uninitialized, 1);
  assert_bottom(STRLEN(uninitialized, 0), uninitialized);

  CHAR_ARRAY(s, 2);
  IF_NONDET(s[0], 1);
  s[1] = 0;
  CHAR_PTR(p);
  p = s;
  RES z2 = STRLEN(p, 0); // alarm
  //@ assert (z2 == 1);

  CHAR_ARRAY(t, 4);
  t[0] = t[1] = 10;
  IF_NONDET(t[2], 10);
  t[3] = 0;
  p = t;
  RES z3 = STRLEN(p, 0); // alarm
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
  assert_bottom(strlen(p), p);
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
  int z1 = strlen(p);
  //@assert (z1 == 2);
}

void init_array_nondet(char *a, int from, int to, int val1, int val2) {
  int val = NONDET(val1, val2);
  memset(a + from, val, to-from+1);
  from = to = val1 = val2 = -1; // reset to minimize oracle changes
}

//DELPHIC_TEST
void strlen_large() {
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 99, 1, 2);
  a[20] = 0;
  a[75] = 0;
  Ival offset = RANGE(3, 30);
  RES z1 = STRLEN(a, offset);
  //@ assert (z1 >= 0 && z1 <= 54);

  offset = RANGE(5, 17);
  RES z2 = STRLEN(a, offset);
  //@ assert (z2 >= 3 && z2 <= 15);

  offset = RANGE(60, 74);
  RES z3 = STRLEN(a, offset);
  //@ assert (z3 >= 1 && z3 <= 15);

  offset = RANGE(63, 80);
  RES z4 = STRLEN(a, offset); // alarm
  //@ assert (z4 >= 0 && z4 <= 12);

  init_array_nondet(a, 0, 99, 0, 2);
  offset = RANGE(50, 70);
  RES z5 = STRLEN(a, offset); // alarm
  //@ assert (z5 >= 0 && z5 <= 49);

}

//DELPHIC_TEST
void strlen_large_uninit() {
  CHAR_ARRAY(a, 100);
  init_array_nondet(a, 0, 39, 1, 2);
  init_array_nondet(a, 50, 94, 3, 4);
  a[20] = 0;
  a[75] = 0;

  Ival offset = RANGE(3, 30);
  RES z1 = STRLEN(a, offset); // alarm
  //@ assert (z1 >= 0 && z1 <= 17);

  a[98] = 0;
  offset = RANGE(63, 80);
  RES z2 = STRLEN(a, offset); // alarm
  //@ assert (z2 >= 0 && z2 <= 12);

  offset = RANGE(45, 55);
  RES z3 = STRLEN(a, offset); // alarm
  //@ assert (z3 >= 20 && z3 <= 25);

  offset = 0; // avoid oracle diffs when changed
}

void escaping() {
  char s[4];
  {
    int x;
    *((int *)s) = (int) &x;
  }
  if (nondet) s[0] = 0;
  int z1 = strlen(s); // alarm
  //@ assert (z1 == 0);
  s[0] = 0;
  int z2 = strlen(s); // no alarm
  //@ assert (z2 == 0);
}

void misc() {
  const char* loc_str = "Bonjour Monde\n";
  char loc_char_array[5];
  size_t sz1,sz2,sz3,sz4,sz5,sz6,sz7,sz8;
  int x = 0xabcdef00;
  int z = 0x12345600;
  int i;
  char *str;

  assert_bottom(strlen(unterminated_string), unterminated_string);

  str = nondet ? static_str : loc_str;
  sz1 = strlen(str);  
  //@ assert(sz1 == 12) || (sz1 == 14);
  str = &x;
  str = nondet ? str : str + 3;
  sz2 = strlen(str);
  //@ assert(sz2 == 0) ; // no, could also do an RTE
  i = Frama_C_interval(0,TSZ-1);
  str = tab_str[i];
  sz3 = strlen(str);
  //@ assert (sz3 >= 0) && (sz3 <= 13);
  loc_char_array[3] = '\0';
  assert_bottom(strlen(loc_char_array), loc_char_array);
  sz4 = strlen(zero_str);
  //@ assert(sz4 == 3);
  char *s1 = nondet ? "abc" : "ABCD";
  char *s2 = nondet ? s1 : s1+1;
  sz5 = strlen(s2);
  //@ assert(sz5 >= 2 && sz5 <= 4);

  s1 = nondet ? "efg" : "EFGH";
  s2 = nondet ? s1+1 : s1+2;
  sz6 = strlen(s2);
  //@ assert(sz6 >= 1 && sz6 <= 3);

  s1 = nondet ? "mno\0pqr" : "MNOP\0QRS";
  for (int j = 0; j < 8; j++) {
    sz7 = strlen(s1 + j);
    //@ assert(sz7 >= 0 && sz7 <= 4);
  }

  char maybe_init[2];
  maybe_init[1] = '\0';
  if (nondet) maybe_init[0] = 'A';
  sz8 = strlen(maybe_init);
  //@ assert(sz8 == 1);
}

void big_array () {
  int t[1000000];
  int u[200];
  int r[201];
  int *p;
  p = &t[nondet];
  *p = 0x10230067;
  p = &u[nondet];
  *p = 0x10230067;
  p = &r[nondet];
  *p = 0x10230067;

  unsigned long len_u;
  unsigned long len_r;
  unsigned long len_t;

  len_u = strlen((char const *)u); // below plevel; precise
  len_r = strlen((char const *)r); // above plevel; imprecise
  len_t = strlen((char const *)t); // *far* above plevel
  Frama_C_show_each(len_u, len_r, len_t);

  len_u = strlen((char const *)(u+nondet)); // should be precise
  len_r = strlen((char const *)(r+nondet));
  len_t = strlen((char const *)(t+nondet));
  Frama_C_show_each(len_u, len_r, len_t);
}

void negative_offsets() {
  char buf[100];
  for (int i = 0; i < 100; i++) buf[i] = 'A'; //avoid memset due to C++ oracles
  buf[99] = 0;
  unsigned len1, len2, len3, len4, len5, len6;
  if (nondet) {
    int offset1 = Frama_C_interval(-10, -8);
    len1 = strlen(buf + offset1);
  }
  if (nondet) {
    int offset2 = Frama_C_interval(-2, -1);
    len2 = strlen(buf + offset2);
  }
  int offset3 = Frama_C_interval(-1, -0);
  len3 = strlen(buf + offset3);
  int offset4 = Frama_C_interval(-1, 2);
  len4 = strlen(buf + offset4);
  int offset5 = Frama_C_interval(-4, 7);
  len5 = strlen(buf + offset5);
  int offset6 = Frama_C_interval(-10, 0);
  char *p = buf + offset6;
  len6 = strlen(p);
  char dest[100 * 2];
}

int main (int c) {
  small_sets();
  zero_termination();
  strlen_initialization();
  strlen_large();
  strlen_large_uninit();
  misc();
  bitfields();
  bitfields2();
  escaping();
  big_array();
  negative_offsets();
  return 0;
}
