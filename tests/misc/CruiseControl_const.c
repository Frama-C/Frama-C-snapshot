/* run.config
   GCC:
   DONTRUN:
*/
/*$************* SCADE_KCG KCG Version 5.1.1 (build i10) **************
** Command :
** l2C        CruiseControl.lus -node CruiseControl
**     -noexp @ALL@
**     -keep_named_var
**     -const
**     -bitwise
**     -loc_ctx
**     -no_copy_mem
**     -debug
** date of generation (MM/DD/YYYY): 07/06/2007 13:30:09
** last modification date for CruiseControl.lus (MM/DD/YYYY): 07/06/2007 
********************************************************************$*/

#include "CruiseControl.h"
const Speed ZeroSpeed = (real) 0.0;

const Speed SpeedInc = (real) 2.0;

const Speed SpeedMax = (real) 150.0;

const Speed SpeedMin = (real) 30.0;

const Percent ZeroPercent = (real) 0.0;

const real Kp = (real) 8.113;

const real Ki = (real) 0.5;

const Percent RegThrottleMax = (real) 45.0;



/*$************* SCADE_KCG KCG Version 5.1.1 (build i10) **************
** End of file CruiseControl_const.c
** End of generation (MM/DD/YYYY) : 07/06/2007 13:30:09
********************************************************************$*/
