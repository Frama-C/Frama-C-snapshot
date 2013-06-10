/* run.config 
   OPT: -rte -warn-signed-overflow -print -rte-precond
*/

typedef double typetab[2];
/*@ requires \valid(t) && \valid_range(&*t,0,0) && \valid_range((double *)t,0,1); */ 
double g4(typetab *t)
{
double y;
y = (*t)[0] + (*t)[1];
return y;
}
 
/*@ assigns \nothing; */
double f4()
{
double tab[2],r;
tab[0]=1.0;
tab[1]=2.0;
r = g4( &tab );
return r;
}
