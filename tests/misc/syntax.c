/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -ulevel 22 -journal-disable
*/

// #include <stdio.h>
int a;
int t[25];
int main()
{
  int i;
  for (i=-10; i< 10; i++)
    {
      t[i+10] = (int*)(i+10)-(int*)10;
      }
}



//
