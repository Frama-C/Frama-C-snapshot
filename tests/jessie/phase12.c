typedef unsigned int T_WORD32  ;

typedef enum {
	NOT_OK = 0, 
	OK = 1
} T_RESULTAT;

extern T_WORD32 __ConsignerPanne_cpt ;
extern const T_RESULTAT *__ConsignerPanne_v0 ;

extern T_WORD32 __TT0_cpt ;
extern T_RESULTAT *__TT0_v0 ;

extern T_WORD32 __TT1_cpt ;
extern T_RESULTAT *__TT1_v0 ;

/*@ predicate f_cond0{L}(T_RESULTAT *testv0, integer cpt) =
  @   testv0[cpt] == OK;
  @*/

/*@ predicate f_cond1{L}(T_RESULTAT *testv0, integer cpt) =
  @   testv0[cpt] == NOT_OK;
  @*/

/*@ predicate f_cond_OK{L}(T_RESULTAT *testv0, integer cpt, integer cpt_panne) =
  @   f_cond0{L}(testv0, cpt)
  @   || f_cond1{L}(testv0, cpt)
  @     && __ConsignerPanne_v0[cpt_panne] == OK;
  @   // reads testv0[cpt], __ConsignerPanne_v0[cpt_panne];
  @*/

typedef enum {TEST_T0, TEST_T1} T_ID_TEST ;

/*@ requires \valid(status);
  @ assigns *status, __ConsignerPanne_cpt, __TT0_cpt, __TT1_cpt;
  @ ensures \old(__ConsignerPanne_cpt) <= __ConsignerPanne_cpt <= \old(__ConsignerPanne_cpt) + 1;
  @ behavior T0:
  @   assumes id_test == TEST_T0 && *status == OK;
  @   assigns *status, __ConsignerPanne_cpt, __TT0_cpt;
  @   ensures f_cond_OK{Here}(__TT0_v0, \old(__TT0_cpt), \old(__ConsignerPanne_cpt))
  @     ==> *status == OK;
  @ behavior T1:
  @   assumes id_test == TEST_T1 && *status == OK;
  @   assigns *status, __ConsignerPanne_cpt, __TT1_cpt;
  @   ensures f_cond_OK{Here}(__TT1_v0, \old(__TT1_cpt), \old(__ConsignerPanne_cpt))
  @     ==> *status == OK;
  @*/
T_RESULTAT TesterOK (T_RESULTAT* status, T_ID_TEST id_test);

//@ ghost T_WORD32 __ConsignerPanne_cpt_t0;
//@ ghost T_WORD32 __ConsignerPanne_cpt_t1;

/*@ requires \valid_range(__TT0_v0, __TT0_cpt, __TT0_cpt+1);
  @ requires \valid_range(__TT1_v0, __TT1_cpt, __TT1_cpt+1);
  @ requires \valid_range(__ConsignerPanne_v0, __ConsignerPanne_cpt, __ConsignerPanne_cpt+1);
  @ ensures f_cond_OK{Here}(__TT0_v0, \old(__TT0_cpt), __ConsignerPanne_cpt_t0)
  @   && f_cond_OK{Here}(__TT1_v0, \old(__TT1_cpt), __ConsignerPanne_cpt_t1)
  @     ==> \result == OK;
  @*/
T_RESULTAT phase1()
{
  T_RESULTAT Rl_Status = OK ;

  //@ ghost __ConsignerPanne_cpt_t0 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T0) ;
  /*@ assert 
    @   f_cond_OK{Here}(__TT0_v0, \at(__TT0_cpt,Pre), __ConsignerPanne_cpt_t0)
    @     ==> Rl_Status == OK; */
  //@ ghost __ConsignerPanne_cpt_t1 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T1) ;
  return Rl_Status ;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make -j phase12"
End:
*/
