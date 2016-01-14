
int A,B;

/*@
  assigns A,B ;
  behavior Case:
  assumes \at(A,Here) > 0;
  ensures B > 0;
*/
void job(void);


void call(void)
{
  A = 1;
  job();
  //@ assert B > 0;
  /* SHALL BE PROVED (unknown in BTS 2040) */
}
