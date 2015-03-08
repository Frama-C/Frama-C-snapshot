/* run.config_qualif
   OPT: -wp -wp-par 1
*/

/* Incorrect translation of logic constant.
   Runtime needs a memory for creating C-variable location, which leads to
   dummy memory state in constant definition of [&p]. */

typedef struct { int a; } las;
las * p;
//@ logic las** p_ref = &p;
/*@ requires \valid(p); assigns p->a; */
void f3() { p->a = 7; }
/*@ requires \valid(p); assigns (*p_ref)->a; */
void g3()
{ f3(); }
