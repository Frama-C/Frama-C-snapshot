/*@
axiomatic A {
 predicate P{L}(void (*galois_fp)());

 predicate Q{L, L2}(void (*galois_fp_old)());
 }
*/
/*@ requires P{Pre}(\at(fp,Pre));
   ensures Q{Pre, Post}(\at(fp, Pre));
*/
long f0(void (*fp)(void))
{
   return 0;
}

/*@ requires P{Pre}(\at(fp,Pre));
   ensures Q{Pre, Post}(\at(fp, Pre));
*/
long f1(void (*fp)(int))
{
   return 0;
}

/*@
axiomatic A1 {
 predicate P1{L}(void (*galois_fp)(void));

 predicate Q1{L, L2}(void (*galois_fp_old)(void));
 }
*/
/*@ requires P1{Pre}(\at(fp,Pre));
   ensures Q1{Pre, Post}(\at(fp, Pre));
*/
long f2(void (*fp)(void))
{
   return 0;
}

/*@ requires ill_typed: P1{Pre}(\at(fp,Pre));
   ensures ill_typed: Q1{Pre, Post}(\at(fp, Pre));
*/
long f3(void (*fp)(int))
{
   return 0;
}

void my_f() { return; }

/*@ lemma OK{L}: P(my_f) && P1(my_f); */
