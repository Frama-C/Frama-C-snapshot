void main (int c) {
  int G=0,i;
  int MAX = 12;
  int JMAX=5;
  int j;
  /*@ loop pragma UNROLL_LOOP 14; */ // first loop unrolled 14 times
  for (i=0; i<=MAX; i++)
    {
      G+=i;
    }
  /*@ loop pragma UNROLL_LOOP 124; */
  for (i=0; i<=10*MAX; i++)
    {
      G+=i;
    }
  /*@ loop pragma UNROLL_LOOP 14; */
  for (i=0; i<=MAX; i++)
    {
      j=0;
      /*@ loop pragma UNROLL_LOOP 50; */
      while (j<=JMAX)
        {
          G+=i;
          j++;
          }
    }

//@ loop pragma UNROLL_LOOP 128;
  do {
    G += i;
    i++;
    j--;
    }
  while (i<=256 || j>=0);

//@ loop pragma UNROLL_LOOP 10;
 do
    { if(c) continue;

    if(c--) goto L;
    c++;
  L: c++;
      }
  while(c);

//@ loop pragma UNROLL_LOOP c;
 while(0);

}

#if 0
struct T { unsigned long long addr;
  unsigned long long size;
  unsigned long type; } t_biosmap[10];

struct T * const g_biosmap = t_biosmap;
struct T * biosmap;
int main2(int c,signed char nr_map) {
  biosmap = g_biosmap;
  if (nr_map<2)  return (-1);

//@ loop pragma UNROLL_LOOP 200;
  do {
    unsigned long long start = biosmap->addr;
    unsigned long long size = biosmap->size;
    unsigned long long end = start + size;
    unsigned long type = biosmap->type;
    CEA_F(nr_map);
    if (start>end) return -1;
    if (c) {
      start = 0x100000L;
      size = end - start; continue; };
    }
  while (biosmap++,--nr_map);

  return 0;
}
#endif
