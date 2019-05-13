/* run.config
   OPT: -wp-model +real
   OPT: -wp-model +float
*/

/* run.config_qualif
   OPT: -wp-model +real
   OPT: -wp-model +float
*/

// OK with +real, KO with +float

/*@
  ensures \result <==> (\abs(x-y) < 1e-5) ;
*/
static int dequal(double const x, double const y)
{
  double tmp = x-y ;
  if ( tmp < 1e-5 && tmp > -1e-5 )
    return 1;
  else
    return 0;
}
