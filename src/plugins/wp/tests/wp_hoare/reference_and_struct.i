/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
  OPT: -wp-model +ref
*/

struct T {int a; int b;};

/*@ requires \valid(p);
    assigns p->a;
    ensures p->a == 0;
*/
void reset (struct T *p) {p->a = 0;}

struct T st ;

/*@  assigns st.a; 
     ensures \result == 0; 
 */
int call_reset (void)
{ reset(&st) ; return (st.a);}


/*@ requires \valid(fp+(0..4)); 
    assigns fp[0..4];
    ensures Reset5: \forall integer i; 0<=i<5 ==> (fp+i)->a == 0;
 */
void reset_5 (struct T *fp);


struct T ts[10];

/*@  assigns ts[0..4]; 
     ensures Preset_5: \forall integer i; 0<=i<5 ==> ts[i].a == 0; 
*/
void call_reset_5 (void)
{ reset_5(ts) ; return ;}


struct T * tps[10];

/*@ requires \valid(tps[9]+(0..4)); 
    assigns tps[9][0..4];
    ensures Preset_5_tps : \forall integer i; 0<=i<5 ==> tps[9][i].a == 0; 
 */
void call_reset_5_tps(void)
{ reset_5(tps[9]); return;}



/*@ requires \valid(rp[0]+(0..4)); 
    assigns rp[0][0..4];
    ensures 
      \forall integer i; 0<= i<5 ==> (rp[0]+i)->a == 0;
 */
void reset_1_5 (struct T (*rp) [5]);

struct T smatrix [20] [5] ;

/*@ 
   assigns smatrix[0][0..4]; 
   ensures Presset_mat : \forall integer i; 0<= i<5 ==> (smatrix[0]+i)->a == 0;
 */
void call_reset_1_5(void)
{reset_1_5(smatrix); return;}


/*@ 
   assigns smatrix[1][0..4]; 
   ensures Presset_mat : \forall integer i; 0<= i<5 ==> (smatrix[1]+i)->a == 0;
 */
void call_reset_5_dim2(void)
{reset_5(smatrix[1]); return;}

/*-------------------------------------------------------*/

struct S { int tab[10]; int k;};
struct S s;

int reg_load[5];
/*@ requires \valid(hp+(0..4)); 
    assigns reg_load[0..4];
    ensures \forall integer i; 
            0<=i<5 ==> reg_load[i] == hp[i];
 */
void load_5 (int * hp);

/*@
  assigns reg_load[0..4];
  ensures Pload : \forall integer i; 0<=i<5 ==> reg_load[i] == \old(s.tab[i]);
*/ 
void call_on_array_in_struct_global (void){load_5(s.tab);}

/*@
 requires \valid(sf.tab+(0..4));
 assigns reg_load[0..4];
  ensures Pload2 : \forall integer j; 0<=j<5 ==> reg_load[j] == sf.tab[j];
  ensures Pload3 : \forall integer j; 0<=j<5 ==> \result.tab[j] == sf.tab[j];
*/ 
struct S call_array_in_struct_param(struct S sf){load_5(sf.tab); return sf;}

