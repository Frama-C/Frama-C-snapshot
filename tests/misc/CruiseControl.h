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

#ifndef _INCLUDE_SCADE_TYPES
#    include "scade_types.h"
#endif

#include "definitions.h"


/* ======================== */
/* CONTEXT for node CruiseSpeedMgt */
/* ======================== */

/* Type for context */
typedef struct {
    bool _I0_On;
    bool _I1_Set;
    bool _I2_QuickAccel;
    bool _I3_QuickDecel;
    Speed _I4_Speed;
    Speed _O0_CruiseSpeed;
    Speed _L1_CruiseControl;
    bool _L2_CruiseControl;
    bool _L3_CruiseControl;
    bool _L6_CruiseControl;
    Speed _L21_CruiseControl;
    Speed _L10_CruiseControl;
    Speed _L12_CruiseControl;
    real _L13_CruiseControl;
    Speed _L15_CruiseControl;
    bool _L16_CruiseControl;
    bool _L4_CruiseControl;
    bool _L17_CruiseControl;
    Speed _L11_CruiseControl;
    real _L14_CruiseControl;
    Speed _L19_CruiseControl;
    bool _L18_CruiseControl;
    bool _L5_CruiseControl;
    bool _L20_CruiseControl;
    real _L9_CruiseControl;
    real _L8_CruiseControl;
    Speed _L7_CruiseControl;
    bool _M_init_0_CruiseControl;
} _C_CruiseSpeedMgt;

/* ======================== */
/* CONTEXT for node SaturateThrottle */
/* ======================== */

/* Type for context */
typedef struct {
    Percent _I0_ThrottleIn;
    Percent _O0_ThrottleOut;
    bool _O1_Saturate;
    Percent _L18_CruiseControl;
    Percent _L12_CruiseControl;
    bool _L7_CruiseControl;
    Percent _L17_CruiseControl;
    bool _L9_CruiseControl;
    Percent _L6_CruiseControl;
    Percent _L8_CruiseControl;
    bool _L13_CruiseControl;
} _C_SaturateThrottle;

/* ======================== */
/* CONTEXT for node ThrottleRegulation */
/* ======================== */

/* Type for context */
typedef struct {
    bool _I0_Reset;
    Speed _I1_CruiseSpeed;
    Speed _I2_VehiculeSpeed;
    Percent _O0_Throttle;
    Speed _L1_CruiseControl;
    Speed _L2_CruiseControl;
    real _L3_CruiseControl;
    real _L6_CruiseControl;
    real ProportionnalAction;
    Speed _L22_CruiseControl;
    bool HoldIntegralAction;
    Speed _L16_CruiseControl;
    Speed _L23_CruiseControl;
    Speed _L18_CruiseControl;
    real _L10_CruiseControl;
    real _L8_CruiseControl;
    real IntegralAction;
    real _L4_CruiseControl;
    Percent _L13_CruiseControl;
    bool _L14_CruiseControl;
    bool _L19_CruiseControl;
    Speed _L21_CruiseControl;
    bool _M_init_0_CruiseControl;
    _C_SaturateThrottle _C0_SaturateThrottle;
} _C_ThrottleRegulation;

/* ======================== */
/* CONTEXT for node ThrottleCmd */
/* ======================== */

/* Type for context */
typedef struct {
    bool _I0_Regul_ON;
    Speed _I1_CruiseSpeed;
    Speed _I2_VehiculeSpeed;
    Percent _I3_Accelerator;
    Percent _O0_Throttle;
    bool _L21_CruiseControl;
    bool _L20_CruiseControl;
    bool _L22_CruiseControl;
    bool ONRisingEdge;
    Percent _L26_CruiseControl;
    Speed _L1_CruiseControl;
    Speed _L2_CruiseControl;
    Percent _L19_CruiseControl;
    Percent _L25_CruiseControl;
    Percent _L24_CruiseControl;
    bool _M_init_0_CruiseControl;
    _C_ThrottleRegulation _C0_ThrottleRegulation;
    bool _M_condact_2_CruiseControl;
} _C_ThrottleCmd;

/* ======================== */
/* CONTEXT for node CruiseStateMgt */
/* ======================== */

/* Type for context */
typedef struct {
    bool _I0_BrakePressed;
    bool _I1_AcceleratorPressed;
    bool _I2_Resume;
    bool _I3_On;
    bool _I4_Off;
    bool _I5_SpeedOutOffLimits;
    bool _O0_Regul_ON;
    bool _O1_Regul_OFF;
    bool _O2_Regul_STDBY;
    bool _LE24_CruiseControl;
    bool _LE0_CruiseControl;
    bool _LE4_CruiseControl;
    bool _LE40_CruiseControl;
    bool _LE26_CruiseControl;
    bool _LE28_CruiseControl;
    bool _LE9_CruiseControl;
    bool _LE10_CruiseControl;
    bool _LE11_CruiseControl;
    bool _LE33_CruiseControl;
    bool _LE35_CruiseControl;
    bool _LE38_CruiseControl;
    bool _LE12_CruiseControl;
    bool _LE13_CruiseControl;
    bool _LE14_CruiseControl;
    bool _LE17_CruiseControl;
    bool _LE18_CruiseControl;
    bool _LE3_CruiseControl;
    bool _LE16_CruiseControl;
    bool _LE41_CruiseControl;
    bool _LE19_CruiseControl;
    bool _LE20_CruiseControl;
    bool _LE21_CruiseControl;
    bool _LE1_CruiseControl;
    bool _LE5_CruiseControl;
    bool _LE22_CruiseControl;
    bool _LE23_CruiseControl;
    bool _LE25_CruiseControl;
    bool _LE29_CruiseControl;
    bool _LE2_CruiseControl;
    bool _LE30_CruiseControl;
    bool _LE31_CruiseControl;
    bool _LE32_CruiseControl;
    bool _LE34_CruiseControl;
    bool _LE42_CruiseControl;
    bool _LE6_CruiseControl;
    bool _LE15_CruiseControl;
    bool _LE39_CruiseControl;
    bool _LE43_CruiseControl;
    bool _LE7_CruiseControl;
    bool _LE27_CruiseControl;
    bool _LE36_CruiseControl;
    bool _LE37_CruiseControl;
    bool _LE44_CruiseControl;
    bool _LE8_CruiseControl;
    bool _M_init_0_CruiseControl;
} _C_CruiseStateMgt;

/* ======================== */
/* CONTEXT for node DetectPedalsPressed */
/* ======================== */

/* Type for context */
typedef struct {
    Percent _I0_Brake;
    Percent _I1_Accelerator;
    bool _O0_BrakePressed;
    bool _O1_AcceleratorPressed;
    Percent _L2_CruiseControl;
    Percent _L8_CruiseControl;
    bool _L4_CruiseControl;
    Percent _L1_CruiseControl;
    Percent _L7_CruiseControl;
    bool _L3_CruiseControl;
} _C_DetectPedalsPressed;

/* ======================== */
/* CONTEXT for node DetectSpeedLimits */
/* ======================== */

/* Type for context */
typedef struct {
    Speed _I0_speed;
    bool _O0_SpeedOutOffLimits;
    Speed _L7_CruiseControl;
    Speed _L13_CruiseControl;
    bool _L8_CruiseControl;
    Speed _L14_CruiseControl;
    bool _L9_CruiseControl;
    bool _L17_CruiseControl;
} _C_DetectSpeedLimits;

/* ======================== */
/* CONTEXT for node CruiseControl */
/* ======================== */

/* Type for context */
typedef struct {
    bool _I0_On;
    bool _I1_Off;
    bool _I2_Resume;
    bool _I3_Set;
    bool _I4_QuickAccel;
    bool _I5_QuickDecel;
    Percent _I6_Accel;
    Percent _I7_Brake;
    Speed _I8_Speed;
    Speed _O0_Cruise_speed;
    Percent _O1_Throttle_cmd;
    bool _O2_Regul_ON;
    bool _O3_Regul_OFF;
    bool _O4_Regul_STDBY;
    bool _L73_CruiseControl;
    Percent _L59_CruiseControl;
    Percent _L62_CruiseControl;
    bool BrakePressed;
    bool AcceleratorPressed;
    bool _L61_CruiseControl;
    bool _L60_CruiseControl;
    bool _L58_CruiseControl;
    Speed _L95_CruiseControl;
    bool SpeedOutOffLimits;
    bool _L82_CruiseControl;
    bool _L83_CruiseControl;
    bool _L84_CruiseControl;
    bool _L19_CruiseControl;
    Speed _L96_CruiseControl;
    bool _L38_CruiseControl;
    bool _L39_CruiseControl;
    bool _L40_CruiseControl;
    Speed _L23_CruiseControl;
    Speed CruiseSpeed;
    Percent _L26_CruiseControl;
    Percent _L22_CruiseControl;
    bool _M_init_CruiseControl;
    _C_CruiseSpeedMgt _C0_CruiseSpeedMgt;
    _C_DetectPedalsPressed _C1_DetectPedalsPressed;
    _C_DetectSpeedLimits _C2_DetectSpeedLimits;
    _C_CruiseStateMgt _C3_CruiseStateMgt;
    bool _M_condact_0_CruiseControl;
    _C_ThrottleCmd _C4_ThrottleCmd;
} _C_CruiseControl;

/* ============== */
/* INITIALISATION */
/* ============== */

extern void CruiseSpeedMgt_init(_C_CruiseSpeedMgt *);
extern void SaturateThrottle_init(_C_SaturateThrottle *);
extern void ThrottleRegulation_init(_C_ThrottleRegulation *);
extern void ThrottleCmd_init(_C_ThrottleCmd *);
extern void CruiseStateMgt_init(_C_CruiseStateMgt *);
extern void DetectPedalsPressed_init(_C_DetectPedalsPressed *);
extern void DetectSpeedLimits_init(_C_DetectSpeedLimits *);
extern void CruiseControl_init(_C_CruiseControl *);

/* ================ */
/* CYCLIC FUNCTIONS */
/* ================ */

extern bool CruiseSpeedMgt(_C_CruiseSpeedMgt *);
extern bool SaturateThrottle(_C_SaturateThrottle *);
extern bool ThrottleRegulation(_C_ThrottleRegulation *);
extern bool ThrottleCmd(_C_ThrottleCmd *);
extern bool CruiseStateMgt(_C_CruiseStateMgt *);
extern bool DetectPedalsPressed(_C_DetectPedalsPressed *);
extern bool DetectSpeedLimits(_C_DetectSpeedLimits *);
extern bool CruiseControl(_C_CruiseControl *);


/*$************* SCADE_KCG KCG Version 5.1.1 (build i10) **************
** End of file CruiseControl.h
** End of generation (MM/DD/YYYY) : 07/06/2007 13:30:09
********************************************************************$*/
