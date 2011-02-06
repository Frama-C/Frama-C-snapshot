struct R {int a; int b;};

struct S {struct R f;} ; 


struct R r;
struct S s;

//@  ensures oracle_ok:s == { \old(s) \with .f = r};
int f (void)
{ s.f = r ;
  return s.f.a; 
}

//@  ensures oracle_ok:s == { \old(s) \with .f = r1};
int f2 (struct R r1)
{
  r = r1; 
  s.f = r ; 
  return 1;
}

struct R r3;

//@  ensures oracle_ko: s == { \old(s) \with .f = r3};
int f3_false (struct R r1)
{
  r = r1; 
  s.f = r ; 
  return 1;
}

/*@ ensures oracle_ok: s == { \old(s) \with .f = r1};
  @ ensures oracle_ok: s == { s1 \with .f = r1};
 */
int f4_true (struct S s1, struct R r1)
{
  s1.f = r1; 
  s =s1 ; 
  s1.f = r; 
  return 0;
}

//@ ensures oracle_ko: s == { \old(s) \with .f = r};
int f4_false (struct S s1, struct R r1)
{
  s1.f = r1; 
  s =s1 ; 
  s1.f = r; 
  return 0;
}

//@ ensures oracle_ok: \result == r1;
struct R struct_copy (struct R r1)
{
  struct R tmp;
  tmp.a = r1.a; 
  tmp.b = r1.b; 
  return tmp;
}
