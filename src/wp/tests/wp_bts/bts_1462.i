
void wrong(int c) {
  int x = 0;
  int y = 0;
  /*@ assert for_value: c<= 0 || c == 1 || c>=2; */
  if (c==2) { x=1; y=1; }
 L:
  /*@ 
    loop invariant A_KO: \at(x==0,L) ==> i!=0 ==> y == 0;
    loop invariant B: \at(x==1,L) ==> i!=0 ==> x == 1;
    loop invariant C: \at(c==0,Pre) ==> i==0 ==> x == 0;
    loop assigns i,x,y;
   */
  for (int i = 0; i<10; i++) {
    if (c == 0) { x = 0; }
    if (c == 1) { y = 1; }
    if (c == 2) { x = 1; }
  }
  if (c==1) { /*@ assert consequence_of_false_invariant: y==0; */ }
}


void local (void)
{
  int y;
  /*@ loop invariant \valid(&y); loop assigns y; */
  for(;;)
    {
      int x;
      x++;
      y++;
    }
}
