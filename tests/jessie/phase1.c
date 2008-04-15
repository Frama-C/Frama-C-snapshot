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

extern T_WORD32 __TT2_cpt ;
extern T_RESULTAT *__TT2_v0 ;

extern T_WORD32 __TT3_cpt ;
extern T_RESULTAT *__TT3_v0 ;

extern T_WORD32 __TT4_cpt ;
extern T_RESULTAT *__TT4_v0 ;

extern T_WORD32 __TT5_cpt ;
extern T_RESULTAT *__TT5_v0 ;

extern T_WORD32 __TT6_cpt ;
extern T_RESULTAT *__TT6_v0 ;

extern T_WORD32 __TT7_cpt ;
extern T_RESULTAT *__TT7_v0 ;

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
  @ //predicate f_cond_OK{L}(T_RESULTAT *testv0, int cpt, int cpt_panne) reads testv0[cpt], __ConsignerPanne_v0[cpt_panne];
  @*/

typedef enum {TEST_T0, TEST_T1, TEST_T2, TEST_T3, TEST_T4, TEST_T5, TEST_T6, TEST_T7} T_ID_TEST ;

/*@ requires \valid(status);
  @ assigns *status, __ConsignerPanne_cpt, __TT0_cpt, __TT1_cpt, __TT2_cpt,
  @   __TT3_cpt, __TT4_cpt, __TT5_cpt,
  @   __TT6_cpt, __TT7_cpt;
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
  @ behavior T2:
  @   assumes id_test == TEST_T2 && *status == OK;
  @   assigns *status, __ConsignerPanne_cpt, __TT2_cpt;
  @   ensures f_cond_OK{Here}(__TT2_v0, \old(__TT2_cpt), \old(__ConsignerPanne_cpt))
  @     ==> *status == OK;
  @ behavior T3:
  @   assumes id_test == TEST_T3 && *status == OK;
  @   assigns *status, __ConsignerPanne_cpt, __TT3_cpt;
  @   ensures f_cond_OK{Here}(__TT3_v0, \old(__TT3_cpt), \old(__ConsignerPanne_cpt))
  @     ==> *status == OK;
  @ behavior T4:
  @   assumes id_test == TEST_T4 && *status == OK;
  @   assigns *status, __ConsignerPanne_cpt, __TT4_cpt;
  @   ensures f_cond_OK{Here}(__TT4_v0, \old(__TT4_cpt), \old(__ConsignerPanne_cpt))
  @     ==> *status == OK;
  @ behavior T5:
  @   assumes id_test == TEST_T5 && *status == OK;
  @   assigns *status, __ConsignerPanne_cpt, __TT5_cpt;
  @   ensures f_cond_OK{Here}(__TT5_v0, \old(__TT5_cpt), \old(__ConsignerPanne_cpt))
  @     ==> *status == OK;
  @ behavior T6:
  @   assumes id_test == TEST_T6 && *status == OK;
  @   assigns *status, __ConsignerPanne_cpt, __TT6_cpt;
  @   ensures f_cond_OK{Here}(__TT6_v0, \old(__TT6_cpt), \old(__ConsignerPanne_cpt))
  @     ==> *status == OK;
  @ behavior T7:
  @   assumes id_test == TEST_T7 && *status == OK;
  @   assigns *status, __ConsignerPanne_cpt, __TT7_cpt;
  @   ensures f_cond_OK{Here}(__TT7_v0, \old(__TT7_cpt), \old(__ConsignerPanne_cpt))
  @     ==> *status == OK;
  @*/
T_RESULTAT TesterOK (T_RESULTAT* status, T_ID_TEST id_test);

//@ ghost T_WORD32 __ConsignerPanne_cpt_t0;
//@ ghost T_WORD32 __ConsignerPanne_cpt_t1;
//@ ghost T_WORD32 __ConsignerPanne_cpt_t2;
//@ ghost T_WORD32 __ConsignerPanne_cpt_t3;
//@ ghost T_WORD32 __ConsignerPanne_cpt_t4;
//@ ghost T_WORD32 __ConsignerPanne_cpt_t5;
//@ ghost T_WORD32 __ConsignerPanne_cpt_t6;
//@ ghost T_WORD32 __ConsignerPanne_cpt_t7;

/*@ requires \valid_range(__TT0_v0, __TT0_cpt, __TT0_cpt+1);
  @ requires \valid_range(__TT1_v0, __TT1_cpt, __TT1_cpt+1);
  @ requires \valid_range(__TT2_v0, __TT2_cpt, __TT2_cpt+1);
  @ requires \valid_range(__TT3_v0, __TT3_cpt, __TT3_cpt+1);
  @ requires \valid_range(__TT4_v0, __TT4_cpt, __TT4_cpt+1);
  @ requires \valid_range(__TT5_v0, __TT5_cpt, __TT5_cpt+1);
  @ requires \valid_range(__TT6_v0, __TT6_cpt, __TT6_cpt+1);
  @ requires \valid_range(__TT7_v0, __TT7_cpt, __TT7_cpt+1);
  @ requires \valid_range(__ConsignerPanne_v0, __ConsignerPanne_cpt, __ConsignerPanne_cpt+7);
  @ ensures f_cond_OK{Here}(__TT0_v0, \old(__TT0_cpt), __ConsignerPanne_cpt_t0)
  @   && f_cond_OK{Here}(__TT1_v0, \old(__TT1_cpt), __ConsignerPanne_cpt_t1)
  @   && f_cond_OK{Here}(__TT2_v0, \old(__TT2_cpt), __ConsignerPanne_cpt_t2)
  @   && f_cond_OK{Here}(__TT3_v0, \old(__TT3_cpt), __ConsignerPanne_cpt_t3)
  @   && f_cond_OK{Here}(__TT4_v0, \old(__TT4_cpt), __ConsignerPanne_cpt_t4)
  @   && f_cond_OK{Here}(__TT5_v0, \old(__TT5_cpt), __ConsignerPanne_cpt_t5)
  @   && f_cond_OK{Here}(__TT6_v0, \old(__TT6_cpt), __ConsignerPanne_cpt_t6)
  @   && f_cond_OK{Here}(__TT7_v0, \old(__TT7_cpt), __ConsignerPanne_cpt_t7)
  @     ==> \result == OK;
  @*/
T_RESULTAT phase1()
{
  T_RESULTAT Rl_Status = OK ;

  //@ ghost __ConsignerPanne_cpt_t0 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T0) ;
  /* assert 
    @  f_cond_OK{Here}(__TT0_v0, \at(__TT0_cpt,Pre), __ConsignerPanne_cpt_t0)
    @     ==> Rl_Status == OK; */
  //@ ghost __ConsignerPanne_cpt_t1 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T1) ;
  /* assert 
    @  f_cond_OK{Here}(__TT0_v0, \at(__TT0_cpt,Pre), __ConsignerPanne_cpt_t0)
    @  && f_cond_OK{Here}(__TT1_v0, \at(__TT1_cpt,Pre), __ConsignerPanne_cpt_t1)
    @     ==> Rl_Status == OK; */
  //@ ghost __ConsignerPanne_cpt_t2 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T2) ;
  /* assert 
    @  f_cond_OK{Here}(__TT0_v0, \at(__TT0_cpt,Pre), __ConsignerPanne_cpt_t0)
    @  && f_cond_OK{Here}(__TT1_v0, \at(__TT1_cpt,Pre), __ConsignerPanne_cpt_t1)
    @  && f_cond_OK{Here}(__TT2_v0, \at(__TT2_cpt,Pre), __ConsignerPanne_cpt_t2)
    @     ==> Rl_Status == OK; */
  //@ ghost __ConsignerPanne_cpt_t3 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T3) ;
  /* assert 
    @  f_cond_OK{Here}(__TT0_v0, \at(__TT0_cpt,Pre), __ConsignerPanne_cpt_t0)
    @  && f_cond_OK{Here}(__TT1_v0, \at(__TT1_cpt,Pre), __ConsignerPanne_cpt_t1)
    @  && f_cond_OK{Here}(__TT2_v0, \at(__TT2_cpt,Pre), __ConsignerPanne_cpt_t2)
    @  && f_cond_OK{Here}(__TT3_v0, \at(__TT3_cpt,Pre), __ConsignerPanne_cpt_t3)
    @     ==> Rl_Status == OK; */
  //@ ghost __ConsignerPanne_cpt_t4 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T4) ;
  /* assert 
    @  f_cond_OK{Here}(__TT0_v0, \at(__TT0_cpt,Pre), __ConsignerPanne_cpt_t0)
    @  && f_cond_OK{Here}(__TT1_v0, \at(__TT1_cpt,Pre), __ConsignerPanne_cpt_t1)
    @  && f_cond_OK{Here}(__TT2_v0, \at(__TT2_cpt,Pre), __ConsignerPanne_cpt_t2)
    @  && f_cond_OK{Here}(__TT3_v0, \at(__TT3_cpt,Pre), __ConsignerPanne_cpt_t3)
    @  && f_cond_OK{Here}(__TT4_v0, \at(__TT4_cpt,Pre), __ConsignerPanne_cpt_t4)
    @     ==> Rl_Status == OK; */
  //@ ghost __ConsignerPanne_cpt_t5 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T5) ;
  /* assert 
    @  f_cond_OK{Here}(__TT0_v0, \at(__TT0_cpt,Pre), __ConsignerPanne_cpt_t0)
    @  && f_cond_OK{Here}(__TT1_v0, \at(__TT1_cpt,Pre), __ConsignerPanne_cpt_t1)
    @  && f_cond_OK{Here}(__TT2_v0, \at(__TT2_cpt,Pre), __ConsignerPanne_cpt_t2)
    @  && f_cond_OK{Here}(__TT3_v0, \at(__TT3_cpt,Pre), __ConsignerPanne_cpt_t3)
    @  && f_cond_OK{Here}(__TT4_v0, \at(__TT4_cpt,Pre), __ConsignerPanne_cpt_t4)
    @  && f_cond_OK{Here}(__TT5_v0, \at(__TT5_cpt,Pre), __ConsignerPanne_cpt_t5)
    @     ==> Rl_Status == OK; */
  //@ ghost __ConsignerPanne_cpt_t6 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T6) ;
  /* assert 
    @  f_cond_OK{Here}(__TT0_v0, \at(__TT0_cpt,Pre), __ConsignerPanne_cpt_t0)
    @  && f_cond_OK{Here}(__TT1_v0, \at(__TT1_cpt,Pre), __ConsignerPanne_cpt_t1)
    @  && f_cond_OK{Here}(__TT2_v0, \at(__TT2_cpt,Pre), __ConsignerPanne_cpt_t2)
    @  && f_cond_OK{Here}(__TT3_v0, \at(__TT3_cpt,Pre), __ConsignerPanne_cpt_t3)
    @  && f_cond_OK{Here}(__TT4_v0, \at(__TT4_cpt,Pre), __ConsignerPanne_cpt_t4)
    @  && f_cond_OK{Here}(__TT5_v0, \at(__TT5_cpt,Pre), __ConsignerPanne_cpt_t5)
    @  && f_cond_OK{Here}(__TT6_v0, \at(__TT6_cpt,Pre), __ConsignerPanne_cpt_t6)
    @     ==> Rl_Status == OK; */
  //@ ghost __ConsignerPanne_cpt_t7 = __ConsignerPanne_cpt;
  TesterOK(&Rl_Status, TEST_T7) ;
  return Rl_Status ;
}
