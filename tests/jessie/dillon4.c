/*@ axiomatic Cpt {
  @ logic int cpt{L} (double t[],double u,int d,int f) ;//reads t[..],u,d,f;
  @ 
  @ axiom c1{L} : \forall double t[];\forall double u;\forall int i;
  @ i>=0 ==> u>=t[i] ==> cpt{L}(t,u,(int)0,i) == cpt{L}(t,u,(int)0,(int)(i-1)) + (int)1;
  @ 
  @ axiom c2{L} : \forall double t[];\forall double u;\forall int i;
  @ i>=0 ==> u<t[i] ==> cpt{L}(t,u,(int)0,i) == cpt{L}(t,u,(int)0,(int)(i-1));
  @ 
  @ axiom c3{L} : \forall double t[];\forall double u;\forall int i;
  @ i<0 ==> cpt{L}(t,u,(int)0,i) == (int)0;
  @ }
  @*/
 

void NNN(float u,double t[5])
{
  int i;
  int tmp_1;
 
/*@ //GENA: annotations for loop (NNN.c:24)
loop invariant 0 <= i && i <= 5
  && (0 <= i && i <= 5 ==> 5 >= cpt{Here}(t,u,(int)0,(int)(i-1)));
loop assigns i, tmp_1;
loop variant 5 - i;
*/
  for (i = 0; i < 5; i++)
    {
          tmp_1 = 0;
    }
   
}
 
