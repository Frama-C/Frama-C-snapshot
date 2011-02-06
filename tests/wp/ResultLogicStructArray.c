/* 
   kind : Positive
   model name : Store ; bhv : Not Yet Translated
   model name : Hoare ; bhv : Out of Scope
 */

int x,y;
struct S { int * a; };

//@ assigns *(\result.a);
struct S result_field (void)
{
  struct S s; 
  s.a = &x; 
  x=4;
  return s;
}

int main (void) { return 0;}
