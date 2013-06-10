/* run.config 
   OPT: -rte -warn-signed-overflow -print
*/

typedef double typetab[2];

double g4(typetab *t)
{
double y;
y = (*t)[0] + (*t)[1];
return y;
}

double h4(typetab t)
{
  return t[0] + t[1];
} 

double i4(double* t) {
  return t[0] + t[1];
}

/*@ assigns \nothing; */
double f4()
{
double tab[2],r;
tab[0]=1.0;
tab[1]=2.0;
 r = g4( &tab ) ;

return r + h4(tab);
}
