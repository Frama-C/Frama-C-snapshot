
int A[16];
int G;

/*@
    assigns *p;
 */
void f(int *p);

/*@
    assigns A[0];
 */
void jobA(void)
{
  f(A);
}

/*@
    assigns G;
 */
void jobG(void)
{
  f(&G);
}


