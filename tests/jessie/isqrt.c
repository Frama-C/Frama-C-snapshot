// Integer square root, from Hacker's Delight page 203
// (see http://www.hackersdelight.org/)

#include <limits.h>

//@ axiom asr_max_int: \forall integer s; 0 <= s <= 30 ==> 1 <= (INT_MAX >> s);

int isqrt(unsigned x) {
   unsigned x1;
   int s, g0, g1;

   if (x <= 1) return x;
   s = 1;
   x1 = x - 1;
   if (x1 > 65535) {s = s + 8; x1 = x1 >> 16;}
   if (x1 > 255)   {s = s + 4; x1 = x1 >> 8;}
   if (x1 > 15)    {s = s + 2; x1 = x1 >> 4;}
   if (x1 > 3)     {s = s + 1;}

   g0 = 1 << s;                // g0 = 2**s.
   g1 = (g0 + (x >> s)) >> 1;  // g1 = (g0 + x/g0)/2.

   while (g1 < g0) {           // Do while approximations
      g0 = g1;                 // strictly decrease.
      g1 = (g0 + (x/g0)) >> 1;
   }
   return g0;
}

/* 
Local Variables:
compile-command: "LC_ALL=C PPC_OPTIONS=\"-jessie-int-model exact\" make isqrt"
End:
*/
