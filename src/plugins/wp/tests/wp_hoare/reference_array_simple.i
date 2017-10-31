/* run.config
   OPT: -wp-model +ref -wp-msg-key refusage
*/

/* run.config_qualif
   OPT: -wp-model +ref 
*/

int tt[10][5];

//@ ensures \result == p1[i]; assigns \nothing;
int f1 (int * p1,int i) ;

//@ ensures \result ==5; 
int call_f1(void)
{ 
  tt[0][3] = 5; 
  return f1(tt[0],3);
}


//@ ensures \result[3] == p2[j][3] ; assigns \nothing;
int * f2 (int (*p2)[5], int j); 

//@ ensures \result == 5; 
int call_f2(void)
{
  tt[2][3] = 5; 
  return ((f2(tt,2))[3]);
}


//@ ensures \result == p3[k] ; assigns \nothing;
int * f3(int ** p3,int k);


int * tp [10];

//@ ensures \result == tp[5];
int * call_f3(void)
{ return (f3(tp,5));
}
