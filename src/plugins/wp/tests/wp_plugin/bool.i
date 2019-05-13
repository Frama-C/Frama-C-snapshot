/* run.config
   OPT: -wp-no-let
*/

/* run.config_qualif
   OPT: -wp-no-let
*/


/*@ ensures 0 <= \result <= 2 ; */
int job(_Bool a, _Bool b) { return a+b; }

/*@ behavior true:
  @   assumes a == 1 || b == 1;
  @   ensures \result == 1;
  @ behavior false:
  @   assumes !(a == 1 || b == 1);
  @   ensures \result == 0;
 */
_Bool bor_bool(_Bool a, _Bool b) { return (_Bool)(((int)a | (int)b) != 0); }


/*@ behavior true:
  @   assumes a == 1 && b == 1;
  @   ensures \result == 1;
  @ behavior false:
  @   assumes !(a == 1 && b == 1);
  @   ensures \result == 0;
 */
_Bool band_bool(_Bool a, _Bool b) { return (_Bool)(((int)a & (int)b) != 0); }

/*@ behavior true:
  @   assumes (a == 1 && b == 0) || (a == 0 && b == 1);
  @   ensures \result == 1;
  @ behavior false:
  @   assumes !((a == 1 && b == 0) || (a == 0 && b == 1)) ;
  @   ensures \result == 0;
 */
_Bool bxor_bool(_Bool a, _Bool b) { return (_Bool)(((int)a ^ (int)b) != 0); }
