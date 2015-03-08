#include <stddef.h>
unsigned int S;
unsigned int A,B;
#define SIZE 4
#define TESTa(c, s, a) S=s, A=a
#define TESTb(c, s, a, b) S=s,A=a,B=b

//--------------------------------------------------------------------
struct c {
  char ca;
};

static void ct(void) {
  TESTa("c", sizeof(struct c), offsetof(struct c, ca));
  //         : size :  01
  //     gcc :   1  : |a|
}

//--------------------------------------------------------------------
struct d {
  char da;
} __attribute__((__aligned__(SIZE)));

static void dt(void) {
  TESTa("d", sizeof(struct d), offsetof(struct d, da));
  //         : size :  01234
  //     gcc :   4  : |a---|
}

//--------------------------------------------------------------------
struct p {
  char pa __attribute__((__aligned__(SIZE)));
};

static void pt(void) {
  TESTa("p", sizeof(struct p), offsetof(struct p, pa));
  //         : size :  01234
  //     gcc :   4  : |a---|
}

//--------------------------------------------------------------------
struct q {
  char qa __attribute__((__aligned__(SIZE)));
  char qb;
};

static void qt(void) {
  TESTb("q", sizeof(struct q), offsetof(struct q, qa), offsetof(struct q, qb));
  //         : size :  01234
  //     gcc :   4  : |ab--|
}

//--------------------------------------------------------------------
struct r {
  char ra;
  char rb __attribute__((__aligned__(SIZE)));
};

static void rt(void) {
  TESTb("r", sizeof(struct r), offsetof(struct r, ra), offsetof(struct r, rb));
  //         : size :  012345678
  //     gcc :   8  : |a---b---|
}

//--------------------------------------------------------------------
struct s {
  char sa __attribute__((__aligned__(SIZE)));
  char sb __attribute__((__aligned__(SIZE)));
};

static void st(void) {
  TESTb("s", sizeof(struct s), offsetof(struct s, sa), offsetof(struct s, sb));
  //         : size :  012345678
  //     gcc :   8  : |a---b---|
}

//--------------------------------------------------------------------
struct t {
  char ta;
  char tb[0] __attribute__((__aligned__(SIZE)));
};

static void tt(void) {
  TESTb("t", sizeof(struct t), offsetof(struct t, ta), offsetof(struct t, tb));
  //         : size :  012345678 : comment
  //     gcc :   4  : |a---|     : b at offset 4, outside the struct
  // frama-c :   8  : |a---b---| : b of size 1 instead of 0
}

//--------------------------------------------------------------------

int main(void)
{
  ct();
  dt();
  pt();
  qt();
  rt();
  st();
  tt();
  return 0;
}
