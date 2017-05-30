/*run.config
  STDOPT: #"-machdep gcc_x86_64 -kernel-msg-key cabs2cil:pragma"
  STDOPT: #"-cpp-command=\"gcc -E -C -I. -m32\" -cpp-frama-c-compliant"
  STDOPT: #"-machdep msvc_x86_64"
 */

#include "pragma-pack-utils.h"

#include <stdint.h>
#include <stddef.h>

#if defined(_MSC_VER) && _MSC_VER > 0
// __attribute__ is not allowed in MSVC, so we remove them.
// this must be done after including some files from the Frama-C stdlib,
// to ensure that _MSC_VER is defined.
# define __attribute__(...)
#endif

#define DEF_STRUCT(NAME)                        \
  struct NAME {                                 \
    signed char i;                              \
    long double j;                              \
  }

#define DEF_STRUCT_WITH_ATTRS(NAME,...)         \
  struct NAME##_ATTR {                          \
    signed char i;                              \
    long double j;                              \
  } __attribute__((__VA_ARGS__))

// test functions are declared here to minimize shifting oracles in case of changes
void tests1(); void tests2();

#ifdef __GNUC__
// This main function is not used when testing MSVC on Visual C++;
// instead, _tmain (defined at the end) is used
int main() {
  tests1(); tests2();
  return 0;
}
#endif

// before any pragma pack
DEF_STRUCT(S);

DEF_STRUCT_WITH_ATTRS(PACK8,__packed__,__aligned__(8));
DEF_STRUCT_WITH_ATTRS(PACK1,__packed__,__aligned__(1));
DEF_STRUCT_WITH_ATTRS(PACKPUSH4,__packed__,__aligned__(4));
DEF_STRUCT_WITH_ATTRS(PACK16,__packed__,__aligned__(16));
DEF_STRUCT_WITH_ATTRS(PACK2,__packed__,__aligned__(2));

#pragma pack(8)
DEF_STRUCT(PACK8);

#pragma pack(1)
DEF_STRUCT(PACK1);

#pragma pack(push, 4)
// push alignment 1 and set 4 as current
DEF_STRUCT(PACKPUSH4);

#pragma pack(16)
DEF_STRUCT(PACK16);

#pragma pack(pop)
// popped alignment should be 1
DEF_STRUCT(PACKPOP);

#pragma pack(push)
// pushed and current alignment should be 1
DEF_STRUCT(PACKPUSH);

#pragma pack(2)
DEF_STRUCT(PACK2);

#pragma pack()
// reset to default
DEF_STRUCT(PACKDEF);

// note: pragma pack(pop, <n>) is unsupported by GCC

#pragma pack(pop)
// pop last stacked pragma (1)
DEF_STRUCT(PACKPOPAGAIN);

// this pragma should generate a warning
#pragma pack(pop)
DEF_STRUCT(PACKOVERPOP);

// Macro used to test and print each offset.
// Variables are kept in the function scope so that
// Value will print their values at the end of the function
#define TEST_STRUCT(ST)                                         \
  size_t ST##_i = offsetof(struct ST, i);                       \
  size_t ST##_j = offsetof(struct ST, j);                       \
  size_t ST##_sizeof = sizeof(struct ST);                       \
  PRINTF("  %s_i %s {", STR(ST), IN);                           \
  PRINTF(ZU "}\n", ST##_i);                                     \
  PRINTF("  %s_j %s {", STR(ST), IN);                           \
  PRINTF(ZU "}\n", ST##_j);                                     \
  PRINTF("  %s_sizeof %s {" ZU "}\n", STR(ST), IN, ST##_sizeof)

void tests1() {
  TEST_STRUCT(S);
  TEST_STRUCT(PACK8);
  TEST_STRUCT(PACK1);
  TEST_STRUCT(PACKPUSH4);
  TEST_STRUCT(PACK16);
  TEST_STRUCT(PACKPOP);
  TEST_STRUCT(PACKPUSH);
  TEST_STRUCT(PACK2);
  TEST_STRUCT(PACKDEF);
  TEST_STRUCT(PACKPOPAGAIN);
  TEST_STRUCT(PACKOVERPOP);

  TEST_STRUCT(PACK8_ATTR);
  TEST_STRUCT(PACK1_ATTR);
  TEST_STRUCT(PACKPUSH4_ATTR);
  TEST_STRUCT(PACK16_ATTR);
  TEST_STRUCT(PACK2_ATTR);
}

// END OF FIRST ROUND OF TESTS


// SECOND ROUND OF TESTS:
// each struct is defined inside a #pragma directive and
// then, outside of it, the "equivalent" version that should
// be produced by Frama-C. Both should output the same values
// when compiled with GCC and MSVC.

// restore alignment to default
#pragma pack()

#pragma pack(push,4)
typedef struct {
  signed char i;
  signed char j __attribute__((__aligned__(2)));
  long double k;
  char l;
} test1;
#pragma pack(pop)

typedef struct {
  signed char i __attribute__((__aligned__(4)));
  signed char j __attribute__((__aligned__(2<4?2:4)));
  long double k __attribute__((__aligned__(4)));
  char l __attribute__((__aligned__(4)));
} __attribute__((__packed__)) emul1;


#pragma pack(push,1)
typedef struct {
  signed char i;
  signed char j __attribute__((__aligned__(2)));
  long double k;
  char l;
} test2;
#pragma pack(pop)

typedef struct {
  signed char i __attribute__((__aligned__(1)));
  signed char j __attribute__((__aligned__(2<1?2:1)));
  long double k __attribute__((__aligned__(1)));
  char l __attribute__((__aligned__(1)));
} __attribute__((__packed__)) emul2;


#pragma pack(push,4)
typedef struct {
  signed char i;
  signed char j;
  long double k;
  char l;
} __attribute__((__packed__)) test3;
#pragma pack(pop)

typedef struct {
  signed char i;
  signed char j;
  long double k;
  char l;
} __attribute__((__packed__)) emul3;


#pragma pack(push,2)
typedef struct {
  signed char i;
  signed char j __attribute__((__aligned__(4)));
  long double k;
  char l;
} __attribute__((__packed__)) test4;
#pragma pack(pop)

typedef struct {
  signed char i;
  signed char j __attribute__((__aligned__(2<4?2:4)));
  long double k;
  char l;
} __attribute__((__packed__)) emul4;


#pragma pack(push,2)
typedef struct {
  signed char i;
  signed char j __attribute__((__aligned__));
  long double k;
  char l;
} __attribute__((__packed__)) test5;
#pragma pack(pop)

typedef struct {
  signed char i;
  signed char j __attribute__((__aligned__(2))); // minimum between "max_align" and 2
  long double k;
  char l;
} __attribute__((__packed__)) emul5;


#pragma pack(push,2)
typedef struct {
  signed char i;
  signed char j __attribute__((__aligned__(1))) __attribute__((__aligned__(4)));
  long double k;
  char l;
} __attribute__((__packed__)) test6;
#pragma pack(pop)

typedef struct {
  signed char i;
  signed char j __attribute__((__aligned__(2)));
  long double k;
  char l;
} __attribute__((__packed__)) emul6;


#pragma pack(push,2)
typedef struct {
  signed char i;
  signed long j __attribute__((__aligned__(1)));
  signed char q;
  signed short p;
  long double k;
  char l;
} test7;
typedef struct {
  test7 i;
  char j;
  long double k;
  test7 l;
} test7_2;
#pragma pack(pop)

#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))
// N below must be synchronized with the pragma pack above
#define N 2

typedef struct {
  signed char i __attribute__((__aligned__(MIN(sizeof(signed char),N))));
  signed long j __attribute__((__aligned__(MIN(MAX(sizeof(signed long),1),N))));
  signed char q __attribute__((__aligned__(MIN(sizeof(signed char),N))));
  signed short p __attribute__((__aligned__(MIN(sizeof(signed short),N))));
  long double k __attribute__((__aligned__(MIN(sizeof(long double),N))));
  char l __attribute__((__aligned__(MIN(sizeof(char),N))));
} __attribute__((__packed__)) emul7;
typedef struct {
  emul7 i __attribute__((__aligned__(2))) ;
  char j __attribute__((__aligned__(2))) ;
  long double k __attribute__((__aligned__(2))) ;
  emul7 l __attribute__((__aligned__(2))) ;
} __attribute__((__packed__)) emul7_2;

// This test does not test pragma pack, only aligned/packed attributes
typedef struct {
  char i;
  long j __attribute__((__aligned__(2))); // smaller than min, not packed => ignored
  char k;
  long l __attribute__((__packed__,__aligned__(2))); // smaller than min, packed => ok
} test8;

#define TEST(ST)                                                \
  size_t ST##_i = offsetof(ST, i);                              \
  size_t ST##_j = offsetof(ST, j);                              \
  size_t ST##_k = offsetof(ST, k);                              \
  size_t ST##_l = offsetof(ST, l);                              \
  size_t ST##_sizeof = sizeof(ST);                              \
  PRINTF("  %s_i %s {", STR(ST), IN);                           \
  PRINTF(ZU "}\n", ST##_i);                                     \
  PRINTF("  %s_j %s {", STR(ST), IN);                           \
  PRINTF(ZU "}\n", ST##_j);                                     \
  PRINTF("  %s_k %s {", STR(ST), IN);                           \
  PRINTF(ZU "}\n", ST##_k);                                     \
  PRINTF("  %s_l %s {", STR(ST), IN);                           \
  PRINTF(ZU "}\n", ST##_l);                                     \
  PRINTF("  %s_sizeof %s {" ZU "}\n", STR(ST), IN, ST##_sizeof)

void tests2() {
  PRINTF("\n");
  TEST(test1);
  PRINTF("\n");
  TEST(emul1);
  PRINTF("\n");
  TEST(test2);
  PRINTF("\n");
  TEST(emul2);
  PRINTF("\n");
  TEST(test3);
  PRINTF("\n");
  TEST(emul3);
  PRINTF("\n");
  TEST(test4);
  PRINTF("\n");
  TEST(emul4);
  PRINTF("\n");
  TEST(test5);
  PRINTF("\n");
  TEST(emul5);
  PRINTF("\n");
  TEST(test6);
  PRINTF("\n");
  TEST(emul6);
  PRINTF("\n");
  TEST(test7);
  PRINTF("\n");
  TEST(emul7);
  PRINTF("\n");
  TEST(test7_2);
  PRINTF("\n");
  TEST(emul7_2);
  PRINTF("\n");
  TEST(test8);
}

#ifndef __GNUC__
// For MSVC testing on Visual C++
int _tmain(int argc, _TCHAR* argv[]) {
  tests1();
  tests2();
  getchar();
  return 0;
}
#endif
