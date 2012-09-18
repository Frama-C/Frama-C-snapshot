struct S;

/*@ model struct S { integer foo; }; */

/*@ 
  requires \valid(s);
  assigns *s;
  ensures s->foo == 0;
*/
void reset (struct S* s);

/*@ 
  requires \valid(s);
  assigns *s;
  ensures s->foo > \at(s->foo,Pre);
*/
void inc(struct S* s);

/*@ 
  requires \valid(s);
  assigns *s;
  ensures s->foo < \at(s->foo,Pre);
*/
void dec(struct S* s);

/*@  
  requires \valid(s);
  assigns \nothing;
  behavior is_true:
  assumes s->foo > 0;
  ensures \result == 1;
  behavior is_false:
  assumes s->foo <= 0;
  ensures \result == 0;
*/
int is_pos(struct S* s);
