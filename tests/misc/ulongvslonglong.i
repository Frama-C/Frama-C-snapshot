/* run.config
   OPT: -val -journal-disable -machdep x86_64
   OPT: -val -journal-disable
*/

int x;
long x2;
unsigned long x9[6][2];

main(){ 
  x2 = 2793414595;
  for (int i = 0; i < 6; i++)
    {
      for (int j = 0; j < 2; j++)
    x9[i][j] = 1U;
    }
  x = ((0x090E7AF82577C8A6LL | x9[0][1]) <= (~(x2 || x9[0][1])));
  return x;
}
