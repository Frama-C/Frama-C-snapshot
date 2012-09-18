/* run.config
OPT: -load-script tests/syntax/logic_env_script.ml
*/

//@ predicate foo(integer x) = x == 0;

int X;

//@ predicate bar{L} = X == 0;
