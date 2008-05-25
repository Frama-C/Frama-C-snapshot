/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main g
*/
int j;
int t[10];

void g(int i){
  for (i=1;
       i < 1000;
       i++)
    t[i] = j+i++;

  j=1;
  return;
}

