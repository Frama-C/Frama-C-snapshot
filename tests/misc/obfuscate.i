/* run.config
   OPT: -load-module lib/plugins/Obfuscator -obfuscate
*/

int my_var = 0;

/*@ global invariant I: my_var >= 0; */

enum my_enum {
  first, second, third = 4
};

/*@ requires my_var > 0;
    ensures my_var > \old(my_var);
    ensures \forall integer x; x == x; */
int my_func () {
  enum my_enum x = first;
  /*@ assert my_var >= first; */
  my_var++;
  if (!my_var) goto end;
  return my_var + x;
 end: ;
  return -1;
}

/*@ requires \valid(p);
    ensures *p == 0;
*/
void f(int* p);

/*@ behavior bhv:
      exits never: \false;
    complete behaviors bhv;
    disjoint behaviors bhv; */
int logic(int f1)
{
  int V1;
  V1 = 0;
  if (f1) goto end;
  V1 ++;
  /*@ assert property: V1 ? 1: 0; */ ;
  end: ;
  return V1;
}

int main(int* p) { 
  if ("ti\rti" == "ti\rti") f(p); 
}
