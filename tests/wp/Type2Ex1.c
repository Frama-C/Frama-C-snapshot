/* run.config 
   DONTRUN: test under construction
*/

char Buffer[1040] ; 

// note: properties named "oracle_ko" are not true !
/*@

assigns Buffer[..];

exits never: oracle_ok: \false;

behavior b1:
  ensures p1: oracle_ok:
    \forall integer k ; 0 <= k < 3
     ==> Buffer[k] == \old(Buffer[k]);
  ensures p1_ko: oracle_ko:
    \forall integer k ; 0 <= k < 4
     ==> Buffer[k] == \old(Buffer[k]);

behavior b2:
  ensures p2: oracle_ok:
    \forall integer k ; 0 <= k < nb
     ==> Buffer[k+3] == \old(buffer[k]);
  ensures p2_ko: oracle_ko:
    \forall integer k ; 0 <= k < nb
     ==> Buffer[k] == \old(buffer[k]);

behavior b3:
  ensures p3: oracle_ok:
    \forall integer k ; nb+3 <= k < 1040
     ==> Buffer[k] == \old(Buffer[k]);
  ensures p3_ko: oracle_ko:
    \forall integer k ; nb <= k < 1040
     ==> Buffer[k] == \old(Buffer[k]);


 */
void Type2Ex1 (const char * buffer, int nb) 
{
  int i ;
  /*@ loop assigns i, Buffer[3..(3+(i-1))];
      loop invariant i0: oracle_ok: 0 <= i && (0 <= nb ==> i <= nb) ;
      for b1: loop invariant b1: oracle_ok: 
         \forall integer k ; 0 <= k < 3
          ==> Buffer[k] == \at(Buffer[k],Pre);
      for b2: loop invariant b2: oracle_ok: 
         \forall integer k ; 0 <= k < i
          ==> Buffer[k+3] == \at(buffer[k],Pre);
      for b3: loop invariant b3: oracle_ok: 
         \forall integer k ; nb+3 <= k < 1040
          ==> Buffer[k] == \at(Buffer[k],Pre);
      loop variant decr: oracle_ok: nb - i ;
   */
  for (i = 0 ; i < nb ; i++) 
    Buffer[3+i] = buffer[i] ;         
}
