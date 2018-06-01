/* run.config
   STDOPT: +" -cpp-extra-args=-DP0"
   STDOPT: +" -cpp-extra-args=-DP1"
   STDOPT: +" -cpp-extra-args=-DP2"
   STDOPT: +" -cpp-extra-args=-DP3"
   STDOPT: +" -cpp-extra-args=-DP4"
   STDOPT: +" -cpp-extra-args=-DP5"
   STDOPT: +" -cpp-extra-args=-DP6"
   STDOPT: +" -cpp-extra-args=-DP7"
   STDOPT: +" -cpp-extra-args=-DP8"
*/

#ifdef P0
int main(int c)
{
  /*@ ghost //@ requires c >= 0;
      int x = c;
      /@ loop invariant x >= 0;
         loop assigns x;
         loop variant x;
      @/
      while (x > 0) {
        x--;
      }
  */
  return 0;
}
#endif

#ifdef P1
int main()
{
  /*@ ghost
      int x = 10;
      /@ loop invariant x >= 0;
         loop assigns x;
         loop variant x;
      while (x > 0) {
        x--;
      }
  */
  return 0;
}
#endif

#ifdef P2
int main()
{
  /@ assert 2 == 2; @/
  return 0;
}
#endif

#ifdef P3
int main()
{
  assert (2 == 2); @/
  return 0;
}
#endif

#ifdef P4
int main()
{
  //@ assert (2 == 2); @/
  return 0;
}
#endif

#ifdef P5
int main()
{
  /*@ ghost
      int x = 10;
      /@ loop invariant x >= 0;
         /@ loop assigns x; @/
         loop variant x;
      @/
      while (x > 0) {
        x--;
      }
  */
  return 0;
}
#endif

#ifdef P6
int main()
{
  /*@ ghost
      int x = 10;
      /@ loop invariant x >= 0;
         //@ loop assigns x;  // ignored
         loop variant x;
      @/
      while (x > 0) {
        x--;
      }
  */
  return 0;
}
#endif

#ifdef P7
int main(int c)
{
  /*@ ghost //@ requires c >= 0;
      int x = c;
      /@ loop invariant x >= 0;
       @ loop invariant x@==@x;
       @ loop variant x;
       @/
      while (x > 0) {
        x--;
      }
  */
  return 0;
}
#endif

#ifdef P8
int main(int c)
{
  /*@ ghost //@ requires c >= 0;
    @  int x = c;
    @  /@ loop invariant x >= 0;
    @   @ loop invariant x == x;
    @   @ loop variant x;
    @   @/
    @  while (x > 0) {
    @    x--;
    @  }
    @*/
  return 0;
}
#endif
