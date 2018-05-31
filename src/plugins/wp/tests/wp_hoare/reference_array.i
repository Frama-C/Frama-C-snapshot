/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
  OPT: -wp-model +ref
*/

/*@ requires \valid(fp+(0..4)); 
    assigns fp[0..4];
    ensures Reset5: 
      \forall integer i; 0<=i<5 ==> fp[i] == 0;
 */
void reset_5 (int *fp);

int reg_load[5];
int reg_add[5];

/*@ requires \valid(hp+(0..4)); 
    assigns reg_load[0..4];
    ensures \forall integer i; 
            0<=i<5 ==> reg_load[i] == hp[i];
 */
void load_5 (int * hp);

/*@ requires \valid(gp+(0..4)); 
    assigns reg_add[0..4];
    ensures \forall integer i; 
             0<=i<5 ==> reg_add[i] == \old(reg_load[i])+gp[i];
 */
void add_5(int *gp);

/*@ requires \valid(rp[0]+(0..4)); 
    assigns rp[0][0..4];
    ensures 
      \forall integer i; 0<= i<5 ==> rp[0][i] == 0;
 */
void reset_1_5 (int (*rp) [5]) { reset_5(rp[0]);}

/*@ requires \valid(lp[0]+(0..4)); 
    assigns reg_load[0..4];
    ensures \forall integer i; 
            0<=i<5 ==> reg_load[i] == lp[0][i];
 */
void load_1_5 (int (*lp) [5]) {load_5(lp[0]);}

/*@ requires \valid(ap[0]+(0..4)); 
    assigns reg_add[0..4];
    ensures \forall integer i; 
             0<=i<5 ==> reg_add[i] == reg_load[i]+ ap[0][i];
 */
void add_1_5 (int (*ap) [5]) {add_5(ap[0]);}

int t[20]; 
int tt[20][5];

/*@
  assigns reg_load[0..4], reg_add[0..4],t[0..4];
  ensures Pload : \forall integer i; 0<=i<5 ==> reg_load[i] == \old(t[i]);
  ensures Preset: \forall integer j; 0<=j<5 ==> t[j] == 0; 
  ensures Padd  : \forall integer k; 0<=k<5 ==> reg_add[k] ==  \old(t[k]); 
 */
void calls_on_array_dim_1 (void)
{
  load_5(t);
  reset_5(t);
  add_5(t);
}



/*@
  assigns reg_load[0..4], reg_add[0..4],tt[0][0..4];
  ensures Pload :
    \forall integer i; 0<=i<5 ==> reg_load[i] == \old(tt[0][i]);
  ensures Preset: 
    \forall integer j; 0<=j<5 ==> tt[0][j] == 0; 
  ensures Padd  : 
     \forall integer k; 0<=k<5 ==> reg_add[k] ==  \old(tt[0][k]); 
 */
void calls_on_array_dim_2_to_1 (void)
{
  load_5(tt[0]);
  reset_5(tt[0]);
  add_5(tt[0]);
}



/*@
  assigns reg_load[0..4], reg_add[0..4],tt[0][0..4];
  ensures Pload : \forall integer i; 0<=i<5 ==> reg_load[i] == \old(tt[0][i]);
  ensures Preset: \forall integer j; 0<=j<5 ==> tt[0][j] == 0; 
  ensures Padd  : \forall integer k; 0<=k<5 ==> reg_add[k] ==  \old(tt[0][k]); 
 */
void calls_on_array_dim_2 (void)
{
  load_1_5(tt);
  reset_1_5(tt);
  add_1_5(tt);
}




