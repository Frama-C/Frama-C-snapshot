/* run.config
   OPT: -wp-model FLOAT
*/

/* run.config_qualif
   OPT: -wp-model FLOAT
*/

float f ;
double d ;

/*@
  ensures CST_F : 0.5 == 0.5f ;
  ensures CST_D : 0.5 == 0.5d ;
  ensures CNV_F : (float)  0.2 == 0.2f ;
  ensures CNV_D : (double) 0.2 == 0.2d ;
  ensures VAR_F :   f == 0.2f ;
  ensures VAR_D :   d == 0.2d ;
*/
void main() {
  f = 0.2;
  d = 0.2;
}
