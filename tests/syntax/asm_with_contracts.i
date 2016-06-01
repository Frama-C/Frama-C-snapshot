/*@ behavior b:
  assumes z>=0;
  ensures \true;
*/
int f(int z) {

  int x = z;
  int y = 2;
  /*@ assigns y; */
  asm ("mov %1, %0\n\t" : "=r" (y) : "r" (x));

  /*@ for b: assigns x,y; */
  asm ("mov %1, %0\n\t" : "=r" (x) : "r" (y));

  /*@ 
    assigns x,y;
    behavior c:
      assumes x>=0;
      ensures y>=0;
  */
  asm ("mov %1, %0\n\t" : "=r" (y) : "r" (x));

  return x;
}
