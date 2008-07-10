
/* basic types */

char a;
short c;
int b;
long d;
long long e;
float f;
double g;
long double h;

/* void i; GCC refused by 3.2.3 */

/* arrays */

char j[5];
long l[5][6];

/* pointers */

int * m;
short ** n;

/* type specifiers */

extern int o;
const char p;
signed char q;
unsigned int r;
signed s; /* defaults to signed int */

/* misc. (specifiers and pointers) */

unsigned char * * t;
const char** u;

/* structures */

struct S1; /* forward declaration */
struct S2 { int fa; }; 
struct S1 { char fb; float fc; };
struct { struct S1 fe; int ff; } sb;
struct S3 { struct S1 * fg; };
struct S4 { int fh; struct S4 * next; };
struct S4 * sc;

/* bit fields */

struct { int fd :3; } sa;

struct flags { 
  unsigned int is_keyword : 1;
  unsigned int is_extern  : 1;
  unsigned int is_static  : 1;
};

/* unions */

union U1 { int fi; float fj; };
union U2 { union U1 fk; int fl; };

/* mixed structures / unions / arrays */

struct S5 { char fm; union { int fn; float fo; } fp; } arr1[10];

/* enums */

enum boolean { FALSE, TRUE };
enum { E11, E12, E13 };
enum { E21 = 1, E22, E23 };
enum letters { E31 = 'a', E32 = 'b', E33 = 'c', E34 = 'd' };

/* nightmare examples from K&R, section 5.12 */

int (*kra)[13];
int *krb[13];
int *krc();
int (*krd)();
char (*(*kre())[])();
char (*(*krf[3])())[5];

/* typedefs */

typedef int t1;
typedef struct { int fq; } t2;
typedef struct S1 t3;
typedef struct S2 * t4;
typedef int t5[10];

typedef struct t6_ { int fr; struct t6_ * fs; } * t6;
