/* 
   kind : Positive
   model name : Store ; bhv : Provable
   model name : Hoare ; bhv : Proved with alt-ergo
 */

//@ ensures \result == 4;
int res_int_cst(void)
{
  return 4;
}

//@ ensures \result >= 4;
int res_int_cst_lt()
{
  return 4;
}



//@ ensures \result == -4; 
int res_unop_neg(void)
{
  int i = 4; return -i;
}


//@ ensures \result == 4; 
int res_unop_pos(void)
{
  int i = 4; return +i;
}

/*
//@ensures \result == 1 && z == 1; 
int res_unop_not(void) 
{ int z; 
  z = !0; 
  return !4 ; }
*/

//@ ensures \result == 4;
int res_additive_operator(void)
{
  return (4+5-5);
}


//@ ensures \result == 4;
int res_multiplicative_operator(void)
{
  return (4*5/5);
}

//@ ensures \result != 0;
int res_equality(void)
{

  int c,d,e; 
  c=5 ;
  e=3 ; 
  d= (c+e);
  return (d==8);
}


//@ ensures \result == 1; 
int res_relation(void) 
{
  
  int c,d,e; 
  c=5 ;
  e=3 ; 
  d= (c+e);
  return (d>=8);
}
   



int main (void) { return 0 ; }
