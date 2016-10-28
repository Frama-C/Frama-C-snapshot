/* run.config
   OPT:
   OPT: -wp-model +raw
 */

/* run.config_qualif
   OPT:
   OPT: -wp-model +raw
 */

/*@
  assigns \nothing;
*/
void foo(void)
{
  int a;
  int* pa;
  pa = &a;
  *pa = 1;
}

/*@
  assigns \nothing;
*/
void bar(void)
{
  int a;
  a = 1;
}
