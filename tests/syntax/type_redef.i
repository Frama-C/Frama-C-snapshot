/* run.config
   STDOPT:
   STDOPT: #"-c11"
*/ // Note: redefinition of local typedefs is currently unsupported

typedef int myint;
typedef int myint; //valid in C11 only

typedef int list[2];
typedef int list[2]; //valid in C11 only

typedef struct { int a; } st;
typedef struct { int a; } st; //invalid

typedef st st1; //valid

typedef union { int a; } u;
typedef union { int a; } u; //invalid

typedef enum {A} e;
typedef enum {A} e; // invalid

typedef enum {B} e1;
typedef enum {B} e2; // invalid (B redefined)

typedef struct {int a;} st1; //invalid

typedef int I;
void f() {
  typedef int I; //valid (not same scope)
  { typedef int I; }//valid (not same scope)
}

typedef int vi;
typedef volatile int vi; //invalid

typedef int ci;
typedef const int ci; //invalid

typedef __attribute__((aligned(8))) int ai;
typedef int ai; //valid in C11 only

typedef int *ftest_t;
typedef double ftest_t; //invalid

// tests of valid composite type redefinitions
typedef struct _stt { int a; } stt;
typedef struct _stt stt; //valid in C11 only
typedef struct _stt2 stt; //invalid
typedef struct _stt stt2; //valid

void g() {
  typedef struct _stt { int a; } stt; //valid
}

void h() {
  typedef struct _stt stt; //valid
}

typedef int magic;
void i() {
  typedef void (*magic)(void); //valid
  { typedef struct {int obj;} magic; } //valid
  magic m = g; //valid (test scoping of local typedef)
}
magic m = 2; //valid (test scoping of local typedef)

void main(ftest_t i) { }
