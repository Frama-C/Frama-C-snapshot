
typedef struct _Decomp2 { char c1; char c2; } Decomp2;
typedef union _Integer2 { short x; Decomp2 c; } Integer2;

/*@ requires \valid(i); 
  @ ensures i->x == \result->x;
  @*/
Integer2* endianness(Integer2 *i) {
  Integer2 *j = (Integer2*)malloc(sizeof(Integer2));
  j->c.c1 = i->c.c1;
  j->c.c2 = i->c.c2;
  return j;
}

typedef struct _Decomp { char c1; char c2; char c3; char c4; } Decomp;
typedef union _Integer { int x; Decomp c; } Integer;

//@ ensures i.x == \result.x;
Integer endianness2(Integer i) {
  Integer j;
  j.c.c1 = i.c.c1;
  j.c.c2 = i.c.c2;
  j.c.c3 = i.c.c3;
  j.c.c4 = i.c.c4;
  return j;
}

/*@ requires \valid(i); 
  @ ensures i->x == \result->x;
  @*/
Integer* endianness3(Integer *i) {
  Integer *j = (Integer*)malloc(sizeof(Integer));
  j->c.c1 = i->c.c1;
  j->c.c2 = i->c.c2;
  j->c.c3 = i->c.c3;
  j->c.c4 = i->c.c4;
  return j;
}

typedef struct _S1 { char c; int i; } S1;
typedef struct _S2 { char c; float f; } S2;
typedef union _U { S1 s1; S2 s2; } U;

//@ ensures \result == 1;
int test() {
  U u;
  u.s1.i = 1;
  u.s1.c = 2;
  //@ assert u.s2.c == 2;
  return u.s1.i;
}
  

/* 
Local Variables:
compile-command: "LC_ALL=C make -j union"
End:
*/
