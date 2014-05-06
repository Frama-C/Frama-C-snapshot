/* run.config
  GCC:
  OPT: -val -deps -out -input  -main inst_F6 -absolute-valid-range 0x200-0x199
  OPT: -val -deps -out -input  -main f
*/
void inst_F6(int *v,int n){ int t[3]; t[1] = 4;
 int i,j,ecart,tmp;
 { int i = 0 ; i++ ; j = i; };
 for (ecart = n/2; ecart >0; ecart /=2)
   for (i = ecart; i < n; i++)
     for (j = i-ecart; j>=0 && v[j]>v[j+ecart]; j = j-ecart){
       tmp = v[j];
       v[j] = v[j+ecart];
       v[j+ecart] = tmp;
       }
}

void f(int c) {
  int t[88888];
  t[0] = 99;
  if (c) t[1] = t[100];
}
