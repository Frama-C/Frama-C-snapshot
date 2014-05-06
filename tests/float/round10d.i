/* run.config
   OPT: -val -float-normal -journal-disable -no-results
   OPT: -val -float-normal -all-rounding-modes -journal-disable -no-results
*/
int main()
{
   double t=0.0;
   int i;
   Frama_C_show_each_dixieme(0.1);
   //@ loop pragma UNROLL 10;
   for(i=0;i<10;i++) 
     { 
       t = t + 0.1;
       Frama_C_show_each_t(t);
     }    
   //@ assert t>=1.0;
   return 0;
}
