/* run.config
   OPT: -val-show-slevel 10 -slevel 100 -val -cpp-command "gcc -C -E -DFLOAT=double -DN=55 -I. " -float-normal -journal-disable -no-results
   OPT: -slevel 100 -val -cpp-command "gcc -C -E -DFLOAT=float -DN=26 -I. "  -float-normal -journal-disable -no-results
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
