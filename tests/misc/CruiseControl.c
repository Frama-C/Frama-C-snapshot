/* run.config
   GCC:
   OPT: -float-normal -val -deps -out -input tests/misc/CruiseControl_const.c -lib-entry -main CruiseControl -context-depth 10 -context-valid-pointers -journal-disable
   OPT: -float-hex -all-rounding-modes -val -deps -out -input tests/misc/CruiseControl_const.c -lib-entry -main CruiseControl -context-depth 10 -context-valid-pointers -journal-disable
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
#include "CruiseControl_extern.h"

/* ============== */
/* INITIALISATION */
/* ============== */

void CruiseSpeedMgt_init(_C_CruiseSpeedMgt * _C_)
{
    (_C_->_M_init_0_CruiseControl) = true;
}


void SaturateThrottle_init(_C_SaturateThrottle * _C_)
{
}


void ThrottleRegulation_init(_C_ThrottleRegulation * _C_)
{
    (_C_->_M_init_0_CruiseControl) = true;
    SaturateThrottle_init(&(_C_->_C0_SaturateThrottle));
}


void ThrottleCmd_init(_C_ThrottleCmd * _C_)
{
    (_C_->_M_init_0_CruiseControl) = true;
    ThrottleRegulation_init(&(_C_->_C0_ThrottleRegulation));
    (_C_->_M_condact_2_CruiseControl) = true;
}


void CruiseStateMgt_init(_C_CruiseStateMgt * _C_)
{
    (_C_->_M_init_0_CruiseControl) = true;
}


void DetectPedalsPressed_init(_C_DetectPedalsPressed * _C_)
{
}


void DetectSpeedLimits_init(_C_DetectSpeedLimits * _C_)
{
}


void CruiseControl_init(_C_CruiseControl * _C_)
{
    CruiseSpeedMgt_init(&(_C_->_C0_CruiseSpeedMgt));
    DetectPedalsPressed_init(&(_C_->_C1_DetectPedalsPressed));
    DetectSpeedLimits_init(&(_C_->_C2_DetectSpeedLimits));
    CruiseStateMgt_init(&(_C_->_C3_CruiseStateMgt));
    (_C_->_M_condact_0_CruiseControl) = true;
    ThrottleCmd_init(&(_C_->_C4_ThrottleCmd));
    (_C_->_M_init_CruiseControl) = true;
}

/* ================================*/
/* MAIN NODE (AND UNEXPANDED NODES) */
/* ================================ */

bool CruiseSpeedMgt(_C_CruiseSpeedMgt * _C_)
{
/*#code for node CruiseSpeedMgt */
    (_C_->_L1_CruiseControl) = (_C_->_I4_Speed);
    (_C_->_L2_CruiseControl) = (_C_->_I1_Set);
    (_C_->_L3_CruiseControl) = (_C_->_I0_On);
    (_C_->_L6_CruiseControl) =
	((_C_->_L2_CruiseControl) | (_C_->_L3_CruiseControl));
    (_C_->_L21_CruiseControl) = ZeroSpeed;
    if ((_C_->_M_init_0_CruiseControl)) {
	(_C_->_L10_CruiseControl) = (_C_->_L21_CruiseControl);
    } else {
	(_C_->_L10_CruiseControl) = (_C_->_L7_CruiseControl);
    }
    (_C_->_L12_CruiseControl) = SpeedInc;
    (_C_->_L13_CruiseControl) =
	((_C_->_L10_CruiseControl) + (_C_->_L12_CruiseControl));
    (_C_->_L15_CruiseControl) = SpeedMax;
    (_C_->_L16_CruiseControl) =
	((_C_->_L13_CruiseControl) <= (_C_->_L15_CruiseControl));
    (_C_->_L4_CruiseControl) = (_C_->_I2_QuickAccel);
    (_C_->_L17_CruiseControl) =
	((_C_->_L16_CruiseControl) & (_C_->_L4_CruiseControl));
    (_C_->_L11_CruiseControl) = SpeedInc;
    (_C_->_L14_CruiseControl) =
	((_C_->_L10_CruiseControl) - (_C_->_L11_CruiseControl));
    (_C_->_L19_CruiseControl) = SpeedMin;
    (_C_->_L18_CruiseControl) =
	((_C_->_L14_CruiseControl) >= (_C_->_L19_CruiseControl));
    (_C_->_L5_CruiseControl) = (_C_->_I3_QuickDecel);
    (_C_->_L20_CruiseControl) =
	((_C_->_L18_CruiseControl) & (_C_->_L5_CruiseControl));
    if ((_C_->_L20_CruiseControl)) {
	(_C_->_L9_CruiseControl) = (_C_->_L14_CruiseControl);
    } else {
	(_C_->_L9_CruiseControl) = (_C_->_L10_CruiseControl);
    }
    if ((_C_->_L17_CruiseControl)) {
	(_C_->_L8_CruiseControl) = (_C_->_L13_CruiseControl);
    } else {
	(_C_->_L8_CruiseControl) = (_C_->_L9_CruiseControl);
    }
    if ((_C_->_L6_CruiseControl)) {
	(_C_->_L7_CruiseControl) = (_C_->_L1_CruiseControl);
    } else {
	(_C_->_L7_CruiseControl) = (_C_->_L8_CruiseControl);
    }
    (_C_->_O0_CruiseSpeed) = (_C_->_L7_CruiseControl);
    (_C_->_M_init_0_CruiseControl) = false;
/*#end code for node CruiseSpeedMgt */
    return (true);
}

bool SaturateThrottle(_C_SaturateThrottle * _C_)
{
/*#code for node SaturateThrottle */
    (_C_->_L18_CruiseControl) = RegThrottleMax;
    (_C_->_L12_CruiseControl) = (_C_->_I0_ThrottleIn);
    (_C_->_L7_CruiseControl) =
	((_C_->_L12_CruiseControl) > (_C_->_L18_CruiseControl));
    (_C_->_L17_CruiseControl) = ZeroPercent;
    (_C_->_L9_CruiseControl) =
	((_C_->_L12_CruiseControl) < (_C_->_L17_CruiseControl));
    if ((_C_->_L9_CruiseControl)) {
	(_C_->_L6_CruiseControl) = (_C_->_L17_CruiseControl);
    } else {
	(_C_->_L6_CruiseControl) = (_C_->_L12_CruiseControl);
    }
    if ((_C_->_L7_CruiseControl)) {
	(_C_->_L8_CruiseControl) = (_C_->_L18_CruiseControl);
    } else {
	(_C_->_L8_CruiseControl) = (_C_->_L6_CruiseControl);
    }
    (_C_->_O0_ThrottleOut) = (_C_->_L8_CruiseControl);
    (_C_->_L13_CruiseControl) =
	((_C_->_L9_CruiseControl) | (_C_->_L7_CruiseControl));
    (_C_->_O1_Saturate) = (_C_->_L13_CruiseControl);
/*#end code for node SaturateThrottle */
    //@ assert (_C_->_O1_Saturate == 1) ==> (_C_->_O0_ThrottleOut == ZeroPercent || _C_->_O0_ThrottleOut == RegThrottleMax );
    return (true);
}

bool ThrottleRegulation(_C_ThrottleRegulation * _C_)
{
/*#code for node ThrottleRegulation */
    (_C_->_L1_CruiseControl) = (_C_->_I1_CruiseSpeed);
    (_C_->_L2_CruiseControl) = (_C_->_I2_VehiculeSpeed);
    (_C_->_L3_CruiseControl) =
	((_C_->_L1_CruiseControl) - (_C_->_L2_CruiseControl));
    (_C_->_L6_CruiseControl) = Kp;
    (_C_->ProportionnalAction) =
	((_C_->_L3_CruiseControl) * (_C_->_L6_CruiseControl));
    (_C_->_L22_CruiseControl) = ZeroSpeed;
    if ((_C_->_M_init_0_CruiseControl)) {
	(_C_->HoldIntegralAction) = true;
    } else {
	(_C_->HoldIntegralAction) = (_C_->_L14_CruiseControl);
    }
    if ((_C_->HoldIntegralAction)) {
	(_C_->_L16_CruiseControl) = (_C_->_L22_CruiseControl);
    } else {
	(_C_->_L16_CruiseControl) = (_C_->_L3_CruiseControl);
    }
    (_C_->_L23_CruiseControl) = ZeroSpeed;
    if ((_C_->_M_init_0_CruiseControl)) {
	(_C_->_L18_CruiseControl) = (_C_->_L23_CruiseControl);
    } else {
	(_C_->_L18_CruiseControl) = (_C_->_L21_CruiseControl);
    }
    (_C_->_L10_CruiseControl) =
	((_C_->_L16_CruiseControl) + (_C_->_L18_CruiseControl));
    (_C_->_L8_CruiseControl) = Ki;
    (_C_->IntegralAction) =
	((_C_->_L10_CruiseControl) * (_C_->_L8_CruiseControl));
    (_C_->_L4_CruiseControl) =
	((_C_->ProportionnalAction) + (_C_->IntegralAction));
/* call to node not expanded SaturateThrottle */
    (_C_->_C0_SaturateThrottle._I0_ThrottleIn) = (_C_->_L4_CruiseControl);
    if (!SaturateThrottle(&(_C_->_C0_SaturateThrottle)))
	return (false);
    (_C_->_L13_CruiseControl) =
	(_C_->_C0_SaturateThrottle._O0_ThrottleOut);
    (_C_->_L14_CruiseControl) = (_C_->_C0_SaturateThrottle._O1_Saturate);
    (_C_->_O0_Throttle) = (_C_->_L13_CruiseControl);
    (_C_->_L19_CruiseControl) = (_C_->_I0_Reset);
    if ((_C_->_L19_CruiseControl)) {
	(_C_->_L21_CruiseControl) = (_C_->_L22_CruiseControl);
    } else {
	(_C_->_L21_CruiseControl) = (_C_->_L10_CruiseControl);
    }
    (_C_->_M_init_0_CruiseControl) = false;
/*#end code for node ThrottleRegulation */
    return (true);
}

bool ThrottleCmd(_C_ThrottleCmd * _C_)
{
/*#code for node ThrottleCmd */
    if ((_C_->_M_init_0_CruiseControl)) {
	(_C_->_L21_CruiseControl) = false;
    } else {
	(_C_->_L21_CruiseControl) = (_C_->_L20_CruiseControl);
    }
    (_C_->_L20_CruiseControl) = (_C_->_I0_Regul_ON);
    (_C_->_L22_CruiseControl) = ((_C_->_L21_CruiseControl) ^ true);
    (_C_->ONRisingEdge) =
	((_C_->_L20_CruiseControl) & (_C_->_L22_CruiseControl));
    (_C_->_L26_CruiseControl) = ZeroPercent;
    (_C_->_L1_CruiseControl) = (_C_->_I1_CruiseSpeed);
    (_C_->_L2_CruiseControl) = (_C_->_I2_VehiculeSpeed);
/* begin condact */
    if ((_C_->_L20_CruiseControl)) {
/* call to node not expanded ThrottleRegulation */
	(_C_->_C0_ThrottleRegulation._I0_Reset) = (_C_->ONRisingEdge);
	(_C_->_C0_ThrottleRegulation._I1_CruiseSpeed) =
	    (_C_->_L1_CruiseControl);
	(_C_->_C0_ThrottleRegulation._I2_VehiculeSpeed) =
	    (_C_->_L2_CruiseControl);
	if (!ThrottleRegulation(&(_C_->_C0_ThrottleRegulation)))
	    return (false);
	(_C_->_L19_CruiseControl) =
	    (_C_->_C0_ThrottleRegulation._O0_Throttle);
	(_C_->_M_condact_2_CruiseControl) = false;
    } else {
	if (_C_->_M_init_0_CruiseControl) {
	    (_C_->_L19_CruiseControl) = (_C_->_L26_CruiseControl);
	}
    }
/* end condact */
    (_C_->_L25_CruiseControl) = (_C_->_I3_Accelerator);
    if ((_C_->_L20_CruiseControl)) {
	(_C_->_L24_CruiseControl) = (_C_->_L19_CruiseControl);
    } else {
	(_C_->_L24_CruiseControl) = (_C_->_L25_CruiseControl);
    }
    (_C_->_O0_Throttle) = (_C_->_L24_CruiseControl);
    (_C_->_M_init_0_CruiseControl) = false;
/*#end code for node ThrottleCmd */
    return (true);
}

bool CruiseStateMgt(_C_CruiseStateMgt * _C_)
{
/*#code for node CruiseStateMgt */
    if ((_C_->_M_init_0_CruiseControl)) {
	(_C_->_LE24_CruiseControl) = false;
    } else {
	(_C_->_LE24_CruiseControl) = (_C_->_LE23_CruiseControl);
    }
    (_C_->_LE0_CruiseControl) = (_C_->_I0_BrakePressed);
    (_C_->_LE4_CruiseControl) = (_C_->_I4_Off);
    if ((_C_->_M_init_0_CruiseControl)) {
	(_C_->_LE40_CruiseControl) = false;
	(_C_->_LE26_CruiseControl) = false;
	(_C_->_LE28_CruiseControl) = false;
    } else {
	(_C_->_LE40_CruiseControl) = (_C_->_LE39_CruiseControl);
	(_C_->_LE26_CruiseControl) = (_C_->_LE25_CruiseControl);
	(_C_->_LE28_CruiseControl) = (_C_->_LE27_CruiseControl);
    }
    (_C_->_LE9_CruiseControl) =
	((_C_->_LE26_CruiseControl) | (_C_->_LE28_CruiseControl));
    (_C_->_LE10_CruiseControl) =
	((_C_->_LE24_CruiseControl) | (_C_->_LE9_CruiseControl));
    (_C_->_LE11_CruiseControl) =
	((_C_->_LE40_CruiseControl) | (_C_->_LE10_CruiseControl));
    if ((_C_->_M_init_0_CruiseControl)) {
	(_C_->_LE33_CruiseControl) = false;
	(_C_->_LE35_CruiseControl) = false;
	(_C_->_LE38_CruiseControl) = false;
    } else {
	(_C_->_LE33_CruiseControl) = (_C_->_LE32_CruiseControl);
	(_C_->_LE35_CruiseControl) = (_C_->_LE34_CruiseControl);
	(_C_->_LE38_CruiseControl) = (_C_->_LE37_CruiseControl);
    }
    (_C_->_LE12_CruiseControl) =
	((_C_->_LE35_CruiseControl) | (_C_->_LE38_CruiseControl));
    (_C_->_LE13_CruiseControl) =
	((_C_->_LE33_CruiseControl) | (_C_->_LE12_CruiseControl));
    (_C_->_LE14_CruiseControl) =
	((_C_->_LE11_CruiseControl) | (_C_->_LE13_CruiseControl));
    (_C_->_LE17_CruiseControl) =
	(((_C_->_LE4_CruiseControl) ^ true) & (_C_->_LE14_CruiseControl));
    (_C_->_LE18_CruiseControl) =
	((((_C_->_LE0_CruiseControl) ^ true) & (_C_->
						_LE17_CruiseControl)) &
	 (_C_->_LE10_CruiseControl));
    (_C_->_LE3_CruiseControl) = (_C_->_I3_On);
    if ((_C_->_M_init_0_CruiseControl)) {
	(_C_->_LE16_CruiseControl) = false;
	(_C_->_LE41_CruiseControl) = true;
    } else {
	(_C_->_LE16_CruiseControl) = (_C_->_LE15_CruiseControl);
	(_C_->_LE41_CruiseControl) = (_C_->_LE19_CruiseControl);
    }
    (_C_->_LE19_CruiseControl) =
	((_C_->_LE41_CruiseControl) & ((_C_->_LE3_CruiseControl) ^ true));
    (_C_->_LE20_CruiseControl) =
	(((_C_->_LE3_CruiseControl) & (_C_->
				       _LE16_CruiseControl)) | (((_C_->
								  _LE19_CruiseControl)
								 ^ true) &
								(_C_->
								 _LE41_CruiseControl)));
    (_C_->_LE21_CruiseControl) =
	(((_C_->_LE24_CruiseControl) & (_C_->
					_LE18_CruiseControl)) | (((_C_->
								   _LE0_CruiseControl)
								  ^ true) &
								 (_C_->
								  _LE20_CruiseControl)));
    (_C_->_LE1_CruiseControl) = (_C_->_I1_AcceleratorPressed);
    (_C_->_LE5_CruiseControl) = (_C_->_I5_SpeedOutOffLimits);
    (_C_->_LE22_CruiseControl) =
	(((_C_->_LE1_CruiseControl) ^ true) & ((_C_->
						_LE5_CruiseControl) ^
					       true));
    (_C_->_LE23_CruiseControl) =
	((_C_->_LE21_CruiseControl) & (_C_->_LE22_CruiseControl));
    (_C_->_LE25_CruiseControl) =
	(((_C_->_LE18_CruiseControl) & (_C_->_LE9_CruiseControl)) & (_C_->
								     _LE22_CruiseControl));
    (_C_->_LE29_CruiseControl) =
	((_C_->_LE17_CruiseControl) & (_C_->_LE13_CruiseControl));
    (_C_->_LE2_CruiseControl) = (_C_->_I2_Resume);
    (_C_->_LE30_CruiseControl) =
	((_C_->_LE40_CruiseControl) & (_C_->_LE17_CruiseControl));
    (_C_->_LE31_CruiseControl) =
	(((_C_->_LE33_CruiseControl) & (_C_->
					_LE29_CruiseControl)) | ((_C_->
								  _LE2_CruiseControl)
								 & (_C_->
								    _LE30_CruiseControl)));
    (_C_->_LE32_CruiseControl) =
	((((_C_->_LE0_CruiseControl) ^ true) & (_C_->
						_LE22_CruiseControl)) &
	 (_C_->_LE31_CruiseControl));
    (_C_->_LE34_CruiseControl) =
	(((((_C_->_LE0_CruiseControl) ^ true) & (_C_->
						 _LE29_CruiseControl)) &
	  (_C_->_LE12_CruiseControl)) & (_C_->_LE22_CruiseControl));
    (_C_->_LE42_CruiseControl) =
	((((_C_->_LE23_CruiseControl) | (_C_->
					 _LE25_CruiseControl)) | (_C_->
								  _LE32_CruiseControl))
	 | (_C_->_LE34_CruiseControl));
    (_C_->_LE6_CruiseControl) = (_C_->_LE42_CruiseControl);
    (_C_->_O0_Regul_ON) = (_C_->_LE6_CruiseControl);
    (_C_->_LE15_CruiseControl) =
	((((_C_->_LE3_CruiseControl) ^ true) & (_C_->
						_LE16_CruiseControl)) |
	 ((_C_->_LE4_CruiseControl) & (_C_->_LE14_CruiseControl)));
    (_C_->_LE39_CruiseControl) =
	(((((_C_->_LE0_CruiseControl) & (_C_->
					 _LE29_CruiseControl)) | ((_C_->
								   _LE0_CruiseControl)
								  & (_C_->
								     _LE20_CruiseControl)))
	  | (((_C_->_LE2_CruiseControl) ^ true) &
	     (_C_->_LE30_CruiseControl))) | (((_C_->
					       _LE0_CruiseControl) & (_C_->
								      _LE17_CruiseControl))
					     & (_C_->
						_LE11_CruiseControl)));
    (_C_->_LE43_CruiseControl) =
	(((_C_->_LE19_CruiseControl) | (_C_->_LE15_CruiseControl)) | (_C_->
								      _LE39_CruiseControl));
    (_C_->_LE7_CruiseControl) = (_C_->_LE43_CruiseControl);
    (_C_->_O1_Regul_OFF) = (_C_->_LE7_CruiseControl);
    (_C_->_LE27_CruiseControl) =
	((((_C_->_LE23_CruiseControl) ^ true) & (_C_->
						 _LE21_CruiseControl)) |
	 ((((_C_->_LE25_CruiseControl) ^ true) & (_C_->
						  _LE18_CruiseControl)) &
	  (_C_->_LE9_CruiseControl)));
    (_C_->_LE36_CruiseControl) =
	((((_C_->_LE34_CruiseControl) ^ true) & (_C_->
						 _LE29_CruiseControl)) |
	 ((_C_->_LE2_CruiseControl) & (_C_->_LE30_CruiseControl)));
    (_C_->_LE37_CruiseControl) =
	((((_C_->_LE0_CruiseControl) ^ true) & ((_C_->
						 _LE32_CruiseControl) ^
						true)) & (_C_->
							  _LE36_CruiseControl));
    (_C_->_LE44_CruiseControl) =
	(((_C_->_LE27_CruiseControl) | (_C_->_LE37_CruiseControl)) | (_C_->
								      _LE39_CruiseControl));
    (_C_->_LE8_CruiseControl) = (_C_->_LE44_CruiseControl);
    (_C_->_O2_Regul_STDBY) = (_C_->_LE8_CruiseControl);
    (_C_->_M_init_0_CruiseControl) = false;
/*#end code for node CruiseStateMgt */
    return (true);
}

bool DetectPedalsPressed(_C_DetectPedalsPressed * _C_)
{
/*#code for node DetectPedalsPressed */
    (_C_->_L2_CruiseControl) = (_C_->_I0_Brake);
    (_C_->_L8_CruiseControl) = ZeroPercent;
    (_C_->_L4_CruiseControl) =
	((_C_->_L2_CruiseControl) > (_C_->_L8_CruiseControl));
    (_C_->_O0_BrakePressed) = (_C_->_L4_CruiseControl);
    (_C_->_L1_CruiseControl) = (_C_->_I1_Accelerator);
    (_C_->_L7_CruiseControl) = ZeroPercent;
    (_C_->_L3_CruiseControl) =
	((_C_->_L1_CruiseControl) > (_C_->_L7_CruiseControl));
    (_C_->_O1_AcceleratorPressed) = (_C_->_L3_CruiseControl);
/*#end code for node DetectPedalsPressed */
    return (true);
}

bool DetectSpeedLimits(_C_DetectSpeedLimits * _C_)
{
/*#code for node DetectSpeedLimits */
    (_C_->_L7_CruiseControl) = (_C_->_I0_speed);
    (_C_->_L13_CruiseControl) = SpeedMin;
    (_C_->_L8_CruiseControl) =
	((_C_->_L7_CruiseControl) < (_C_->_L13_CruiseControl));
    (_C_->_L14_CruiseControl) = SpeedMax;
    (_C_->_L9_CruiseControl) =
	((_C_->_L7_CruiseControl) > (_C_->_L14_CruiseControl));
    (_C_->_L17_CruiseControl) =
	((_C_->_L8_CruiseControl) | (_C_->_L9_CruiseControl));
    (_C_->_O0_SpeedOutOffLimits) = (_C_->_L17_CruiseControl);
/*#end code for node DetectSpeedLimits */
    return (true);
}

bool CruiseControl(_C_CruiseControl * _C_)
{
/*#code for node CruiseControl */
    (_C_->_L73_CruiseControl) = (_C_->_I0_On);
    (_C_->_L59_CruiseControl) = (_C_->_I7_Brake);
    (_C_->_L62_CruiseControl) = (_C_->_I6_Accel);
/* call to node not expanded DetectPedalsPressed */
    (_C_->_C1_DetectPedalsPressed._I0_Brake) = (_C_->_L59_CruiseControl);
    (_C_->_C1_DetectPedalsPressed._I1_Accelerator) =
	(_C_->_L62_CruiseControl);
    if (!DetectPedalsPressed(&(_C_->_C1_DetectPedalsPressed)))
	return (false);
    (_C_->BrakePressed) = (_C_->_C1_DetectPedalsPressed._O0_BrakePressed);
    (_C_->AcceleratorPressed) =
	(_C_->_C1_DetectPedalsPressed._O1_AcceleratorPressed);
    (_C_->_L61_CruiseControl) = (_C_->_I2_Resume);
    (_C_->_L60_CruiseControl) = (_C_->_I0_On);
    (_C_->_L58_CruiseControl) = (_C_->_I1_Off);
    (_C_->_L95_CruiseControl) = (_C_->_I8_Speed);
/* call to node not expanded DetectSpeedLimits */
    (_C_->_C2_DetectSpeedLimits._I0_speed) = (_C_->_L95_CruiseControl);
    if (!DetectSpeedLimits(&(_C_->_C2_DetectSpeedLimits)))
	return (false);
    (_C_->SpeedOutOffLimits) =
	(_C_->_C2_DetectSpeedLimits._O0_SpeedOutOffLimits);
/* call to node not expanded CruiseStateMgt */
    (_C_->_C3_CruiseStateMgt._I0_BrakePressed) = (_C_->BrakePressed);
    (_C_->_C3_CruiseStateMgt._I1_AcceleratorPressed) =
	(_C_->AcceleratorPressed);
    (_C_->_C3_CruiseStateMgt._I2_Resume) = (_C_->_L61_CruiseControl);
    (_C_->_C3_CruiseStateMgt._I3_On) = (_C_->_L60_CruiseControl);
    (_C_->_C3_CruiseStateMgt._I4_Off) = (_C_->_L58_CruiseControl);
    (_C_->_C3_CruiseStateMgt._I5_SpeedOutOffLimits) =
	(_C_->SpeedOutOffLimits);
    if (!CruiseStateMgt(&(_C_->_C3_CruiseStateMgt)))
	return (false);
    (_C_->_L82_CruiseControl) = (_C_->_C3_CruiseStateMgt._O0_Regul_ON);
    (_C_->_L83_CruiseControl) = (_C_->_C3_CruiseStateMgt._O1_Regul_OFF);
    (_C_->_L84_CruiseControl) = (_C_->_C3_CruiseStateMgt._O2_Regul_STDBY);
    (_C_->_L19_CruiseControl) =
	((_C_->_L82_CruiseControl) | (_C_->_L84_CruiseControl));
    (_C_->_L96_CruiseControl) = ZeroSpeed;
    (_C_->_L38_CruiseControl) = (_C_->_I3_Set);
    (_C_->_L39_CruiseControl) = (_C_->_I4_QuickAccel);
    (_C_->_L40_CruiseControl) = (_C_->_I5_QuickDecel);
    (_C_->_L23_CruiseControl) = (_C_->_I8_Speed);
/* begin condact */
    if ((_C_->_L19_CruiseControl)) {
/* call to node not expanded CruiseSpeedMgt */
	(_C_->_C0_CruiseSpeedMgt._I0_On) = (_C_->_L73_CruiseControl);
	(_C_->_C0_CruiseSpeedMgt._I1_Set) = (_C_->_L38_CruiseControl);
	(_C_->_C0_CruiseSpeedMgt._I2_QuickAccel) =
	    (_C_->_L39_CruiseControl);
	(_C_->_C0_CruiseSpeedMgt._I3_QuickDecel) =
	    (_C_->_L40_CruiseControl);
	(_C_->_C0_CruiseSpeedMgt._I4_Speed) = (_C_->_L23_CruiseControl);
	if (!CruiseSpeedMgt(&(_C_->_C0_CruiseSpeedMgt)))
	    return (false);
	(_C_->CruiseSpeed) = (_C_->_C0_CruiseSpeedMgt._O0_CruiseSpeed);
	(_C_->_M_condact_0_CruiseControl) = false;
    } else {
	if (_C_->_M_init_CruiseControl) {
	    (_C_->CruiseSpeed) = (_C_->_L96_CruiseControl);
	}
    }
/* end condact */
    (_C_->_O0_Cruise_speed) = (_C_->CruiseSpeed);
    (_C_->_L26_CruiseControl) = (_C_->_I6_Accel);
/* call to node not expanded ThrottleCmd */
    (_C_->_C4_ThrottleCmd._I0_Regul_ON) = (_C_->_L82_CruiseControl);
    (_C_->_C4_ThrottleCmd._I1_CruiseSpeed) = (_C_->CruiseSpeed);
    (_C_->_C4_ThrottleCmd._I2_VehiculeSpeed) = (_C_->_L23_CruiseControl);
    (_C_->_C4_ThrottleCmd._I3_Accelerator) = (_C_->_L26_CruiseControl);
    if (!ThrottleCmd(&(_C_->_C4_ThrottleCmd)))
	return (false);
    (_C_->_L22_CruiseControl) = (_C_->_C4_ThrottleCmd._O0_Throttle);
    (_C_->_O1_Throttle_cmd) = (_C_->_L22_CruiseControl);
    (_C_->_O2_Regul_ON) = (_C_->_L82_CruiseControl);
    (_C_->_O3_Regul_OFF) = (_C_->_L83_CruiseControl);
    (_C_->_O4_Regul_STDBY) = (_C_->_L84_CruiseControl);
    (_C_->_M_init_CruiseControl) = false;
/*#end code for node CruiseControl */
    return (true);
}



/*$************* SCADE_KCG KCG Version 5.1.1 (build i10) **************
** End of file CruiseControl.c
** End of generation (MM/DD/YYYY) : 07/06/2007 13:30:09
********************************************************************$*/
bool main(_C_CruiseControl * _C_){
  CruiseControl_init(_C_);
  while (CruiseControl(_C_));

  return false;
}
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
