/* run.config
   OPT: -wp-model +real
   OPT: -wp-model +float
*/

/* run.config_qualif
   DONTRUN:
*/

//@ predicate P(real x);

float FD,FF ;
double DD,DF ;

/*@
  ensures ACSL_R: P( 2.1 );
  ensures ACSL_F: P( 2.1f );
  ensures ACSL_FR: P( (float) 2.1 );
  ensures ACSL_DR: P( (double) 2.1 );
  ensures ACSL_DF: P( (double) 2.1f );
  ensures C_FD: P( FD );
  ensures C_FF: P( FF );
  ensures C_DD: P( DD );
  ensures C_DF: P( DF );
 */
void job(void)
{
  FD = 2.1 ;
  FF = 2.1f ;
  DD = 2.1 ;
  DF = 2.1f ;
}
