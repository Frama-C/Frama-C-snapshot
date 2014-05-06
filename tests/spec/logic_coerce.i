/* run.config
DONTRUN: fixing lack of coercions in function/data constructors applications
STDOPT: +"-kernel-msg-key printer:logic-coercions"
*/

/*@ logic integer f(integer x) = x + 1; */
/*@ logic integer g(int x) = x + 2; */
/*@ logic int h(int x) = x; */
/*@ lemma trivial: \forall int x; f(x) == g(x) - 1; */
/*@ lemma trivial2: \forall int x; f(x) == h(x) + 1; */

/*@ logic int o(int x) = (int) (x + 2); */
/*@ logic integer o(integer x) = x + 1; */
/*@ lemma overload1: \forall int x; o(x) <= g(x); */
/*@ lemma overload2: \forall integer x; o(x) == f(x); */ 

/*@ type foo_t = Foo(integer); */

/*@ logic foo_t foo(int x) = Foo(x); */
