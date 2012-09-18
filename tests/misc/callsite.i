/* run.config
   OPT: -load-script tests/misc/callsite.ml
 */
// Don't use -debug 1 option in the test command.

void f(void);
void g(void);
void h(void);
void k(void);

void f(void)
{
  g();
  h();
  g();
}

void g(void)
{
  h();
  k();
  h();
}

void h(void)
{
  k();
  k();
}

// Should have 8 call sites:
// CallSites of f : -
// CallSites of g : From f(2)
// CallSites of h : From f(1) + From g (2)
// CallSites of k : From g(1) + From h (2)

