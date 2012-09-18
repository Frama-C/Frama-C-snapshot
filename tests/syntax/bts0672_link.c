/* run.config
   STDOPT: +"tests/syntax/bts0672_link_2.c"
   STDOPT: +"tests/syntax/bts0672_link_2.c" +"-cpp-command 'gcc -C -E -DPROTO'"
*/

int Frama_C_entropy_source;

//@ predicate foo(integer x) = \true;

/*@
  ensures foo(\result);
  assigns \result \from a,b,Frama_C_entropy_source;
  assigns Frama_C_entropy_source \from Frama_C_entropy_source; */
int Frama_C_nondet(int a, int b);

