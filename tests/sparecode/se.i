/* run.config
   STDOPT: +"-sparecode-analysis -lib-entry -main Se"
*/
int glob;

void Se(int e1, int e2, int *es, int *s1, int tab[])
{
    *s1=0;
    glob=10;

    if (e1==0) *s1=1;
    else tab[e1]=5;

    if (*es==1) *es=0;
}
