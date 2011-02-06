/* 
   kind : Positive
   model name : Store ; bhv : Provable
   model name : Hoare ; bhv : Out of Scope
 */
/*@ ensures *x == 3;
    assigns *x;
 */
void ptr_deref_assigns (int * x) {
  *x = 3;
}


int * p; 
int a; 

//@ assigns p, *p , a ; 
/* CORRECT */ 
void ptr_deref_ptr_assigns_1 (void)
{
  p = &a; 
  *p = 5;
}

//@ assigns p,*p; 
/* CORRECT, because ensures p == &a */ 
void ptr_deref_ptr_assigns_2 (void)
{
  p = &a; 
  *p = 5;
}

//@ assigns p; 
/* INCORRECT, because missing a */ 
void ptr_deref_ptr_assigns_3 (void)
{
  p = &a; 
  *p = 5;
}

//@ assigns p , *\old(p); 
/* CORRECT */
void deref_ptr_ptr_assigns_1 (void)
{
  *p=5;
  p=&a;
}

//@ assigns p , *p; 
/* INCORRECT sauf si \old(p==&a) */
void deref_ptr_ptr_assigns_2 (void)
{
  *p=5;
  p=&a;
}


int main () { return 0;}

