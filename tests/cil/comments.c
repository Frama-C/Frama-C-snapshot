/* run.config
   OPT: -print -keep-comments
*/
/* ABC */
void f() {}
//ABD/*FOO*/
/*ABC*/
/*ABC
     */
/*@ requires \true ; // FOO */
void g() {}
