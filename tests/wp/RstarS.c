/* run.config 
   DONTRUN: test under construction
*/

/* note: properties named "oracle_ko" are not true !
*/
/*@ requires \valid(RstarSQ);
    assigns *RstarSQ ;
    ensures \result == *RstarSQ;
 
    behavior b1:
      assumes R==0 && S!=0 ;
      ensures oracle_ok: *RstarSQ!=0;
      ensures oracle_ko: *RstarSQ==0;

    behavior b2:
      assumes R==0 && S==0 ;
      ensures oracle_ok: *RstarSQ==\old(*RstarSQ);
      ensures oracle_ko: *RstarSQ!=\old(*RstarSQ);

    behavior b3:
      assumes R!=0 ;
      ensures oracle_ok: *RstarSQ==0;
      ensures oracle_ko: *RstarSQ!=0;

    complete behaviors ;
    disjoint behaviors ;
*/
int RstarS(int *RstarSQ, int R, int S) {
   if (!R && S)
     *RstarSQ=1;
   else if (R)
     *RstarSQ=0; 
   return *RstarSQ;
}
