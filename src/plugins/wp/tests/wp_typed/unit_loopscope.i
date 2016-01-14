/* run.config_qualif
   OPT:
   OPT: -wp-model +ref
*/



/*@ requires \valid(written); */
void f(unsigned int * written)
{
 int n = 1;
 *written = (unsigned int)0;
 /*@ loop invariant \false ; */
 while (n < 10) {
   n++;
 }
 return;
}
