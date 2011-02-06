struct s {int a;}; 

//@ ensures \result == 4;
int field_eq_load(void)
{
  struct s s1,s2; 
  s1.a =4; 
  s2 = s1; 
  return s2.a;
}

struct s s1,s2;
//@ ensures  s2 == s1; 
void equal_record_test(void)
{
  s2 =s1 ; 
  
}

struct Ts {struct s s1; int t[10];} ; 

struct Ts ts1, ts2; 
//@ ensures \result == ts1.t[0] ;
int equal_with_array_field (void)
{
  ts1=ts2; 
  return ts2.t[0];
}

struct S {int a;}; 

struct S s;


/*@ requires s.a == 5; 
    assigns s.a; 
    ensures \result == {s \with .a = (int)4 }; 
*/
struct S ret_struct(void)
{
  s.a = 4;
  return s;
}
int main (void) { return 0 ; }
