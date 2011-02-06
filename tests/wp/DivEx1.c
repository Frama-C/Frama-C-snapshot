/* run.config 
   DONTRUN: test under construction
*/

typedef struct {int ch11[10]; int ch12;} T1;
typedef struct {int ch21;      T1 ch22;} T2;

/*@ assigns p[0..19].ch21, p[0..19].ch22.ch11[0..9];
    exits never: oracle_ok: \false;

  @ behavior ch21:
      ensures ch21: oracle_ok:
        \forall int k ; 0 <= k < 20 ==> p[k].ch21 == 0 ;
  @ behavior ch22:
      ensures ch22_11: oracle_ok:
        \forall int k,l ;
          0 <= k < 20 && 0 <= l < 10
          ==> p[k].ch22.ch11[l] == 0 ;
      ensures ch22_other: oracle_ok:
        \forall int k ;
          0 <= k < 20 
          ==> p[k].ch22 == {\old(p[k].ch22) \with .ch11=p[k].ch22.ch11} ;
  @*/
void DivEx1 (T2 *p) {
  int i, j ;

  /*@ loop assigns i, j, p[0..i-1].ch21, p[0..i-1].ch22.ch11[0..9];
      loop invariant I0: oracle_ok: 0 <= i <= 20;

    @ for ch21:
        loop invariant reset_21: oracle_ok: 
          \forall int k ; 
            0 <= k < i ==> p[k].ch21 == 0;
    @ for ch22:
        loop invariant old_part_22: oracle_ok:
          \forall int k ;
             0 <= k < i
             ==> p[k].ch22 == {\at(p[k].ch22,Pre) \with .ch11=p[k].ch22.ch11} ;
        loop invariant new_part_22: oracle_ok:
          \forall int k,l ; 
             0 <= k < i && 0 <= l < 10 ==> p[k].ch22.ch11[l] == 0 ;

    @ loop variant Idecr: oracle_ok: 20 - i ;
  */
  for (i = 0 ; i < 20 ; i++) {
    p[i].ch21 = 0 ;
  /*@ loop assigns j, p[i].ch22.ch11[0..j-1];
      loop invariant J0: oracle_ok: 0 <= j <= 10;

    @ for ch22:
        loop invariant reset_11: oracle_ok: 
          \forall int k ; 0 <= k < j ==> p[i].ch22.ch11[k] == 0;

    @ loop variant Jdecr: oracle_ok: 10 - j ;
    @*/
    for (j = 0 ; j < 10 ; j++)
      p[i].ch22.ch11[j] = 0 ;
    }
}

