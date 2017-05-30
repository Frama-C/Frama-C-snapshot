/* run.config
   OPT: -wp-model +ref -wp-msg-key refusage 
*/

/* run.config_qualif
   OPT: -wp-model +ref
*/

/*@ assigns \nothing ; 
    ensures \result == *p ; 
 */
int ref_ctr(int * p); 


int r1 ; 

/*@  assigns r1; 
     ensures Ref_r1 : \result == 4;
 */
int call_ref_ctr (void)
{
  r1 = 4 ;
  return ref_ctr(&r1); 
}

int n1;

/*@ assigns n1; 
    ensures Mem_n1 :\result == 4;
 */
int call_ref_ctr2 (void)
{
  int * s ; s = &n1 ; 
  n1 = 4 ;
  if (ref_ctr(&n1) == 4) return *s; 
  else return n1;  
}

/*----------------------------------------*/

/*@ assigns \nothing ; 
    ensures \result == *q; 
 */
int ref_bd (int * q)
{
  return *q;
}

int r2 ; 

/*@  assigns r2; 
     ensures Ref_r2 : \result == 4;
 */
int call_ref_bd (void)
{
  r2 = 4 ;
  return ref_bd(&r2); 
}


int n2;

/*@ assigns n2; 
    ensures Mem_n2 :\result == 4;
 */
int call_ref_bd2 (void)
{
  int * p ; p = &n2 ; 
  n2 = 4 ;
  if (ref_bd(&n2) == 4) return *p; 
  else return n1;  
}



/*---------------------------------------*/


/*@ requires \valid(p1); 
    assigns \nothing;
    ensures \result == *p1;
 */
int ref_valid (int *p1);

int r7, n4 ;

/*@ assigns r7,n4; 
    ensures R7_N4: \result == 8;
 */
int call_ref_valid(void)
{
  int * p ;
  r7 = 4; n4 = 4; 
  p = &n4; 
  return (ref_valid(&r7)+
          ref_valid(&n4));  

}

/*---------------------------------------*/

/*@  
    assigns \nothing;
    ensures \result == *q1;
 */
int no_ref_bd(int *q1)
{
  int * q ; q = q1+1 ; 
  return *q1;
}

int n5, nr6 ;

/*@ assigns n5,nr6; 
    ensures Mem_n5_nr6 :\result == 8;
 */
int call_no_ref_bd(void)
{
  int * p ;
  n5 = 4; nr6 = 4; 
  p = &n5; 
  return (no_ref_bd(&n5)+
          no_ref_bd(&nr6));  

}

/*---------------------------------------*/

/*@ requires \valid(ref); 
    assigns \nothing ; 
    ensures \result == *ref1 + *ref2;
 */
int ref_ctr_nr(int *ref, int *ref1,int *ref2);

int r5,r6;
int f3,r4;

/*@ requires \valid(&r6);
   assigns f3,r4, r6;
   ensures R_R_R : r5 == 0 ==> \result == 4;
   ensures R_R_R : r5 != 0 ==> \result == 4;
 */
int call_ref_ctr_nr(void)
{ 
  f3 =2 ; r4 = 2  ; r6 = 2; 
  int k; 
  if (r5 == 0) k = f3 ; else k = r6; 
  return ref_ctr_nr(&r5,&k,&r4);
 
}

 
/*---------------------------------------*/

/*@ assigns \nothing ; 
    ensures \result == **pp ; 
 */
int ref_ctr_nstars(int **pp);


/*@ assigns \nothing ; 
    ensures \result == 4; 
 */
int call_ref_ctr_nstars(void)
{
  int x; 
  x = 4 ; 
  int * px = &x; 
  return ref_ctr_nstars(&px);
}


/*-------------------------------*/
int r0; 

/*@ assigns r0; 
    ensures \result == 8;
 */
int call_two_ref(void)
{
  r0 = 4; 
  return (ref_bd(&r0)+ref_ctr(&r0)) ; 
}

/*-------------------------------*/



/*@ 
  requires *pg == 4;
  assigns \nothing; 
  ensures \result == 4 ;
*/
int g (int *pg) {
  return ref_bd(pg);
}

/*-------------------------------*/


struct S { int tab[10]; int k;};
/*@
 requires \valid(sf.tab+(0..5));
 assigns \nothing;
 ensures Pload2 : 
  \forall integer j; 0<=j<5 ==> \result.tab[j] == sf.tab[j] ;
*/ 
struct S array_in_struct_param(struct S sf){return sf;}

