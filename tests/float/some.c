/* run.config
   OPT: -val-show-slevel 10 -slevel 100 -val -cpp-extra-args="-DFLOAT=double -DN=55" -float-normal -journal-disable -no-results
   OPT: -slevel 100 -val -cpp-extra-args="-DFLOAT=float -DN=26"  -float-normal -journal-disable -no-results
*/

FLOAT t[N] = { 1. } ;
FLOAT y = 0.5;

main(){ 
  int i;
  for (i=1 ; i<N; i++)
    {
      t[i] = t[i-1] + y;
      y = y / 2.;
    }
  Frama_C_dump_each();
  return  i;
}
