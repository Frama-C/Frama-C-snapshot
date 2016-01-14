
int t[10][20] ;

/*@ 
  ensures OK1: a!=c ==> t[a][b] == 1 ; 
  ensures OK2: t[c][d] == 2 ;
  ensures KO: t[a][b] == 1 ;
*/
void make(int a,int b,int c,int d)
{
  t[a][b] = 1 ;
  t[c][d] = 2 ;
}
