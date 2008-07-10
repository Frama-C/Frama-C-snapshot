/* run.config
   DONTRUN: static local variables & specifications
 */
typedef int INTEGER;

typedef enum {TRUE=1, FALSE=0} BOOLEAN;


#include "prec_i.h"

extern INTEGER E1[], S1[];

/* @ // proposition avec requires dans les behaviors
  requires k >= 0;
  ensures PREC_I_RE1==E1[k];

  behavior COND_prec_C0 :
   assumes k==0 ;
   requires PREC_I_RE1 == 0;
   ensures S1[0] == 0;

  behavior COND_prec_CN :
   assumes k >= 1;
   requires PREC_I_RE1 == E1[k-1];
   ensures S1[k]==E1[k-1];
 */
/* @ // qui se desucre en:
  requires k >= 0 && (k==0 ==> PREC_I_RE1 == 0) && (k >= 1 ==> PREC_I_RE1 == E1[k-1]);
  ensures PREC_I_RE1==E1[k];

  behavior COND_prec_C0 :
   assumes k==0 ;
   ensures S1[0] == 0;

  behavior COND_prec_CN :
   assumes k >= 1;
   ensures S1[k]==E1[k-1];
 */

/*@ // proposition actuelle
  requires k >= 0;

  behavior default :
   ensures PREC_I_RE1==E1[k];

  behavior COND_prec_C0 :
   assumes k==0 && PREC_I_RE1 == 0;
   ensures S1[0] == 0;

  behavior COND_prec_CN :
   assumes k >= 1 && PREC_I_RE1 == E1[k-1];
   ensures S1[k]==E1[k-1];

 */
void prec(INTEGER k)
{
    PREC_I(0, E1[k], S1[k]);
}


/*@ requires HYPOTHESE_RAM_0_iter_prec : PREC_I_RE1 == 0 ;
 */
void iter_prec()
{
    INTEGER k=0;
/*@
  loop invariant I1_1 : k>=0;
  loop invariant I1_2 : k==0 ==> PREC_I_RE1==0;
  loop invariant I1_3 : k>=1 ==> PREC_I_RE1==E1[k-1];
 */
    while (1)
    {
        prec(k);
        k++;
    }
}

#include "conf1.h"

extern volatile INTEGER M_Horloge_BR;

extern BOOLEAN EB[];
extern BOOLEAN S[];

INTEGER Time_CONF1;

void conf1(INTEGER k)
{
    CONF1(0, EB[k], Time_CONF1, S[k]);
}
void iter_conf1()
{
    INTEGER N=0;
    while (1)
    {
        conf1(N);
        N++;
    }
}
