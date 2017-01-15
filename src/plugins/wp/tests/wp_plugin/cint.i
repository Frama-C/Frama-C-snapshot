/* run.config
   OPT:
   OPT: -no-warn-signed-overflow
   OPT: -warn-unsigned-overflow
   OPT: -warn-signed-downcast
   OPT: -warn-unsigned-downcast
   OPT: -wp-model +nat
*/

/* run.config_qualif
   DONTRUN:
*/

// For each function <F=downcast|overflow>, result should be:
//  - identity when -warn-F, typically R(a,a)
//  - modulus when -no-warn-F, typically R(to_xxx(a),a)
// For bitwise functions, result should be identity for signed and conversion for unsigned,
// whatever the kernel options are (no RTE involved here). With +nat, have only signedness for types hypotheses.
// Default kernel options:
//  -warn-signed-overflow -no-warn-<others>

//@ predicate R(integer x,integer y);

//@ ensures R(\result,a);
int signed_bitwise(int a)
{
  return ~a; // no conversion in *all* models
}

//@ ensures R(\result,a);
short signed_downcast(int a)
{
  return (short) a; // identity *only* for testcases 3 (-warn-signed-downcast)
}

//@ ensures R(\result,a+b);
int signed_overflow(int a,int b)
{
  return a+b; // identity *except* for testcase 1 (-no-warn-signed-overflow)
}

//@ ensures R(\result,a);
unsigned unsigned_bitwise(unsigned a)
{
  return ~a; // converted in *all* models (bit extension), including testcase 5 (nat)
}

//@ ensures R(\result,a);
unsigned unsigned_downcast(int a)
{
  return (unsigned) a; // identity *only* for testcases 4 (-warn-unsigned-downcast)
}

//@ ensures R(\result,a+b);
unsigned unsigned_overflow(unsigned a,unsigned b)
{
  return a+b; // identity *only* for testcases 2 (-warn-unsigned-overflow)
}
