/* run.config 
   DONTRUN: some issues with Cil parsing and pretty printing
*/
#include <stdio.h>

/*OK: Unspecified statements */
int main() {
   struct l {
     struct l** next;
   } s[4];
   struct l* a;
   struct l* p[4];
   struct l* old;
   p[0] = s;
   p[0]->next = &p[0];
   old = (*p[0]->next);
   a = ((*p[0]->next) += 1);
   if (old + 1 != a)
     printf("bug!\n");
   return 0;
}

/*OK: Correct switch */
int foo(int i) {
  switch (i)
    case 0:
    case 1:
    ;
  return 0;
} 

/*KO: Wrong Order of declaration after pretty printing */
extern int base_files[];
const char *const lang_dir_names[] = { "c" };
int base_files[sizeof (lang_dir_names) / sizeof (lang_dir_names[0])];


/*OK: no visible problem. */
struct record {
  int references;
  char buffer[1];
};

typedef struct {
  char * base;
} buffer;

int bar(int flags ) {
  return ((flags & (0x0004))
      ? __builtin_offsetof (struct record, buffer[0])
      : __builtin_offsetof (struct record, references));
}

/*OK: $ in identifiers */
static void
foo$bar() { }


/*OK: typdef void */
typedef void tVoid;
int pimInit(void);

int pimInit(tVoid)
{
  return 1;
}

/*KO: conditional intializers */
#if 0
typedef long int time_t;
struct time_t_is_integer {
  char a[(((time_t) 1.5 == 1)) ? 1 : -1];
}; 
#endif

#if 0
/*KO: complex array size */
int mySize(void) {return 1;}

int main1(void)
{
    union
    {
      int x;
        char a[mySize() * 2];
    } u;

    u.a[0]++;

    return 1;
}
#endif

/*OK: packed attribute prettun printing*/
typedef enum {
 MFT_RECORD_IN_USE = (0x0001),
 MFT_RECORD_IS_DIRECTORY = (0x0002),
 MFT_REC_SPACE_FILLER = 0xffff
} __attribute__ ((__packed__)) MFT_RECORD_FLAGS;

MFT_RECORD_FLAGS foo1;


/*OK: packed attribute parsing */ 

struct __attribute__ ((__packed__)) s;
typedef struct {
  int x1;
  short x2;
  short x3;
} __attribute__ ((__packed__)) s;
s foo2;


/*OK: pretty print an unused label */
void foo3(int z) {
  int i;
 for (i=0; i<10; i++)
   {
        z++;
        if (z < 5)
            continue;
        else
            break;
   }
}


/*OK: local pragma */
void foo4()
{
#pragma TCS_atomic
   printf("hello!");
}

/* OK: bitfields */
typedef struct
{
    int free : 1;
    int counter : 3;
} __attribute__ ((packed)) tpsn_node_t;


typedef struct {
    tpsn_node_t tpsn_node[7];
} __attribute__ ((packed)) app_state_t;


int foo5() {
    app_state_t s;
    s.tpsn_node[0].counter--;
    return 0;
}

/*OK: constfold attributes */
struct swsusp_info {
  unsigned long num_physpages;
  int cpus;
  unsigned long image_pages;
  unsigned long pages;
  unsigned long size;
} __attribute__((aligned((1UL << 12)))); 

struct swsusp_info foo6;


/*OK: attribute on label */
int tcf_exts_dump()
{
  goto rtattr_failure;
  return 0;
 rtattr_failure: __attribute__ ((unused))
    return -1;
}

/*OK: Empty attribute */
int strnvis(char *, const char *, size_t, int) __attribute__ (()) {return 1;}


/*OK: Compound initializer share statements */
struct bars {
     int x;
};
struct foos {
     struct bars b;
     int y;
};

int rand(void);

void foo7(void) {
     int t = rand();
     struct foos f = {
         .b = {
             .x = (t?2:3),
         },
         .y = 42
     };
     return;
}


/*OK: Missing cast */
#include <stdio.h>
#include <stdlib.h>

int foo8(void) {
  char *p;
  int i;
  
  p = malloc(2*sizeof(int));
  *(int *)p       = 1;
  *((int *)p + 1) = 2;
  
  i = *((int *)p)++;
  printf("%d\n", i);
  i = *((int *)p)++;
  printf("%d\n", i);
  
  return 0;
}

/*OK: tricky assigns with cast */
typedef struct tTestStructX
{
    int x;

} tTestStructX;

typedef struct tTestStruct
{
    tTestStructX x;
    int a;
    int b;
    struct tTestStruct* ptr;
    char c;
    short int x16;
    int       x32;

} tTestStruct;

typedef struct tTestStruct2
{
    tTestStructX x;
    int a;
    int b;
    struct tTestStruct2* ptr;
    char c;
    short int x16;
    int       x32;

} tTestStruct2;

void testaccess2(void)
{
    tTestStruct *p = NULL;
    int z;
    tTestStruct t2;
    tTestStruct *p2 = &t2;

    p = (tTestStruct *) modMalloc(sizeof(tTestStruct), 0);
    p->ptr = NULL;
    (tTestStructX*) p2->ptr = (tTestStructX*) p;
}


/*OK: typeOf problem */
struct foo10 {
  int x;
};

struct foo10 foof(int y) {
  return (struct foo10) { y } ;
}

int goo(int z) {
  __typeof__(foof(3)) a = foof(3);
  if (a.x == z) return 1;
  return 0;
}

/*OK: complexity problem in conditionals */
extern __attribute__((const, noreturn)) int ____ilog2_NaN(void);
int __ilog2_u32(int n);
int __ilog2_u64(int n);

#define ilog2(n)				\
(						\
	__builtin_constant_p(n) ? (		\
		(n) < 1 ? ____ilog2_NaN() :	\
		(n) & (1ULL << 63) ? 63 :	\
		(n) & (1ULL << 62) ? 62 :	\
		(n) & (1ULL << 61) ? 61 :	\
		(n) & (1ULL << 60) ? 60 :	\
		(n) & (1ULL << 59) ? 59 :	\
		(n) & (1ULL << 58) ? 58 :	\
		(n) & (1ULL << 57) ? 57 :	\
		(n) & (1ULL << 56) ? 56 :	\
		(n) & (1ULL << 55) ? 55 :	\
		(n) & (1ULL << 54) ? 54 :	\
		(n) & (1ULL << 53) ? 53 :	\
		(n) & (1ULL << 52) ? 52 :	\
		(n) & (1ULL << 51) ? 51 :	\
		(n) & (1ULL << 50) ? 50 :	\
		(n) & (1ULL << 49) ? 49 :	\
		(n) & (1ULL << 48) ? 48 :	\
		(n) & (1ULL << 47) ? 47 :	\
		(n) & (1ULL << 46) ? 46 :	\
		(n) & (1ULL << 45) ? 45 :	\
		(n) & (1ULL << 44) ? 44 :	\
		(n) & (1ULL << 43) ? 43 :	\
		(n) & (1ULL << 42) ? 42 :	\
		(n) & (1ULL << 41) ? 41 :	\
		(n) & (1ULL << 40) ? 40 :	\
		(n) & (1ULL << 39) ? 39 :	\
		(n) & (1ULL << 38) ? 38 :	\
		(n) & (1ULL << 37) ? 37 :	\
		(n) & (1ULL << 36) ? 36 :	\
		(n) & (1ULL << 35) ? 35 :	\
		(n) & (1ULL << 34) ? 34 :	\
		(n) & (1ULL << 33) ? 33 :	\
		(n) & (1ULL << 32) ? 32 :	\
		(n) & (1ULL << 31) ? 31 :	\
		(n) & (1ULL << 30) ? 30 :	\
		(n) & (1ULL << 29) ? 29 :	\
		(n) & (1ULL << 28) ? 28 :	\
		(n) & (1ULL << 27) ? 27 :	\
		(n) & (1ULL << 26) ? 26 :	\
		(n) & (1ULL << 25) ? 25 :	\
		(n) & (1ULL << 24) ? 24 :	\
		(n) & (1ULL << 23) ? 23 :	\
		(n) & (1ULL << 22) ? 22 :	\
		(n) & (1ULL << 21) ? 21 :	\
		(n) & (1ULL << 20) ? 20 :	\
		(n) & (1ULL << 19) ? 19 :	\
		(n) & (1ULL << 18) ? 18 :	\
		(n) & (1ULL << 17) ? 17 :	\
		(n) & (1ULL << 16) ? 16 :	\
		(n) & (1ULL << 15) ? 15 :	\
		(n) & (1ULL << 14) ? 14 :	\
		(n) & (1ULL << 13) ? 13 :	\
		(n) & (1ULL << 12) ? 12 :	\
		(n) & (1ULL << 11) ? 11 :	\
		(n) & (1ULL << 10) ? 10 :	\
		(n) & (1ULL <<  9) ?  9 :	\
		(n) & (1ULL <<  8) ?  8 :	\
		(n) & (1ULL <<  7) ?  7 :	\
		(n) & (1ULL <<  6) ?  6 :	\
		(n) & (1ULL <<  5) ?  5 :	\
		(n) & (1ULL <<  4) ?  4 :	\
		(n) & (1ULL <<  3) ?  3 :	\
		(n) & (1ULL <<  2) ?  2 :	\
		(n) & (1ULL <<  1) ?  1 :	\
		(n) & (1ULL <<  0) ?  0 :	\
		____ilog2_NaN()			\
				   ) :		\
	(sizeof(n) <= 4) ?			\
	__ilog2_u32(n) :			\
	__ilog2_u64(n)				\
 )

void foo11(int n) {
  int x = ilog2(20);
}

#if 0
/*KO: local struct */
int foo12() {
   {
     struct B;
     struct B { double d; };
     {
       struct B;
       extern void bar(struct B d);
       struct B  {
         int k;
         short h;
       };
       struct B p = { 1, 2};
       bar(p);
     }
   }
   return 0;
}
#endif

/*OK: Huge constants */
enum { HUGE_BYTES = (18446744073709551615ULL) == (127 * 2 + 1) };

void foo13() {
  int x = HUGE_BYTES;
}

/*OK: permutation of typedefs */
typedef volatile int mytype_t;  /* line 1 */
typedef int volatile mytype_t;  /* line 2 */
mytype_t my;
typedef struct ATTR {int len;} attr;
typedef attr *pattr;
typedef struct TUPLEDESC {pattr *attrs;} *t;
int foo14(t desc)
{
  pattr *att = desc->attrs;
  return att[0]->len;
}

/*OK: Large constant */
void foo15 (unsigned long long f);

int foo16 (int argc, char **argv)
{
    foo15 (0xFFFFFFFFFFFFFFFFULL); // Should be 16 F's
    return 0;
}

void foo17 (unsigned long long f)
{
}

/*KO: duplicate labels */
int foo18(int a) {
   int x = 0, y = 1;

   (a ? x : y) = ({ 2; goto l ; l: 3;});
   return x;
}
