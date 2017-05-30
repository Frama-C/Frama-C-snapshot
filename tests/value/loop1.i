char U[100]={1, 2};
char NULL_GLOBAL_LOOSING_BITS_ONE_BY_ONE = 0;
void main1 () {
  int i;
  for(i=0;i<100; i++)
    {
      // these assertions are meaningless; used to test if Value does not crash
      /*@ assert i >= \at(i,LoopCurrent); */
      /*@ assert i >= \at(i,LoopEntry); */
      U[i]=7;
    }

}

void main2 () {
  int i;
  for(i=0;i<=100; i++)
    {
      U[i]=7;
    }

}

int main () {
  main1();
  main2();
}
