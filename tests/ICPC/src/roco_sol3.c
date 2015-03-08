#include "stdafx.h"

#include "roco.h"

#ifndef TESTCASE
#include "roco_config.h"
#elif TESTCASE == 1
#include "roco_config_testcase_1.h"
#elif TESTCASE == 2
#include "roco_config_testcase_2.h"
#elif TESTCASE == 3
#include "roco_config_testcase_3.h"
#endif

/* --- control messages --- */

boolean RoCo_activeDesired;
boolean RoCo_commandMoveByAngle;
boolean RoCo_commandMoveToAngle;
boolean RoCo_commandShutOff;
real64  RoCo_desiredDeltaAngle;
real64  RoCo_desiredTargetAngle;
boolean RoCo_moveFast;

/* --- result messages --- */

boolean RoCo_error;
boolean RoCo_isActive;
real64  RoCo_legAngle;
boolean RoCo_legAngleValid;
real64  RoCo_engineVoltage;
boolean RoCo_isAdapted;

/* -- external messages -- */
extern real64  Battery_voltage;
extern real64  Engine_realVoltage;
extern real64  Env_humidity;
extern real64  Env_temperature;
extern boolean Global_emergencyMode;
extern boolean LegSensor_atMark;
extern real64  LegSensor_signal;
extern real64  dT;

/* --- private variables --- */

static real64  angleDiffRequest;
static real64  desiredEngineVoltage;
static boolean initBackwards;
static boolean limitationActive;
static real64  prevAngleSignal;
static real64  rampValue;
static boolean wasActive;
static boolean wasInit;
static boolean enabled;
static real64  rampTarget;
static real64  filteredEnvTemp;
static sint32  direction;
static real64  targetAngle;
static boolean init;
static boolean final;
static boolean deltaSearchPhase;

static Ramp_t  initRamp = { 0, 0.0, 0.0 };
static Timer_t initTimer;
static Ramp_t  moveTimeRamp = { 0, 0.0, 0.0 };;
static PT1_t   envTempFilter;
static Timer_t shutdownTimer;
static real64  voltageFilter;
static Delay_t batteryLowDelay;
static Delay_t standstillDelay;
static real64  adaptation[2] = {0.0, 0.0};
static boolean rampHighReached = FALSE;
static boolean wasRampEnded;


void RoCo_init()
{
    enabled = FALSE;
    angleDiffRequest = 0.0;
    desiredEngineVoltage = 0.0;
    initBackwards = FALSE;
    initTimer = 0.0;
    limitationActive = FALSE;
    prevAngleSignal = 0.0;
    PT1_SetState (&envTempFilter, Env_temperature);
    rampValue = 0.0;
    filteredEnvTemp = 0.0;
    direction = 0;
    targetAngle = 0.0;
    batteryLowDelay = 0.0;
    shutdownTimer = 0.0;
    rampTarget = 0.0;
    voltageFilter = 0.0;
    wasActive = FALSE;
    wasInit = FALSE;
    deltaSearchPhase = FALSE;
    init = FALSE;
    final = FALSE;

    RoCo_activeDesired = FALSE;
    RoCo_commandMoveByAngle = FALSE;
    RoCo_commandMoveToAngle = FALSE;
    RoCo_commandShutOff = FALSE;
    RoCo_desiredDeltaAngle = 0.0;
    RoCo_desiredTargetAngle = 0.0;
    RoCo_moveFast = FALSE;

    RoCo_isActive = FALSE;
    RoCo_legAngle = 0.0;
    RoCo_legAngleValid = FALSE;
    RoCo_error = FALSE;
    RoCo_engineVoltage = 0.0;
    RoCo_isAdapted = FALSE;
}


void RoCo_process(void)
{
    real64  t1 = Battery_voltage;
    real64  t2 = Env_humidity;
    real64  t3 = Env_temperature;
    boolean t4 = LegSensor_atMark;
    real64  t5 = LegSensor_signal;
    real64  t9;
    real64  t10;
    real64  t11;
    Curve_t *t12;
    real64  t13;
    boolean t14;
    real64  t15;

    filteredEnvTemp = PT1_Filter (&envTempFilter, t3 , RoCo_TempFltT_PARAM, dT);
    enabled = ((((filteredEnvTemp > RoCo_envTempLowerLimit_PARAM) &&
                 (filteredEnvTemp < RoCo_envTempUpperLimit_PARAM)) ?
                ((t2 < RoCo_humidityLimit_PARAM) || (!RoCo_checkHumidity_PARAM)) :
                FALSE) &&
               ((!Turn_on_delay (&batteryLowDelay,
                                       t1 < RoCo_batteryLowLimit_PARAM,
                                       RoCo_batteryLowDelay_PARAM, dT)) ||
                (!RoCo_checkBatteryVoltage_PARAM)) &&
               (!RoCo_error));
    if (!enabled) {
        wasActive = wasActive && RoCo_activeDesired;
        RoCo_isActive = FALSE;
        RoCo_engineVoltage = 0.0;
    }
    else if (RoCo_activeDesired || RoCo_isActive) {
        t13 = RoCo_voltageFilter_PARAM;
        t15 = Interpolate_from_curve (&LegSensorSignalToAngle_CURVE, t5);
        RoCo_legAngle = RoCo_legAngle + t15;
        if (t4) {
            RoCo_legAngle = RoCo_angleAtMark_PARAM;
            RoCo_legAngleValid = TRUE;
        }
        if ((RoCo_activeDesired) && (!wasActive)) {
            if (!RoCo_legAngleValid) {
            	init = TRUE;
            }
            RoCo_isActive = TRUE;
        }
        if (!RoCo_activeDesired && wasActive && !init && !final) {
            RoCo_commandShutOff = TRUE;
        }
        if (init) {
        	t14 = (RoCo_angleAtMark_PARAM > RoCo_idlePosition_PARAM);
            if (!wasActive) {
            	Timer_start (&initTimer);
            }
            if ((Timer_elapsedTime (&initTimer) > RoCo_initTimeout_PARAM) ||
            	(Turn_on_delay(&standstillDelay, fabs(t15) < 0.001,
            			RoCo_initStandstillTimeout_PARAM, dT))) {
                if (!initBackwards) {
                    initBackwards = TRUE;
                    Timer_start (&initTimer);
                    Turn_on_delay(&standstillDelay, FALSE,
                    		RoCo_initStandstillTimeout_PARAM, dT);
                }
                else {
                    RoCo_error = TRUE;
                    RoCo_isActive = FALSE;
                    init = FALSE;
                }
            }
            rampTarget = initBackwards ^ t14 ?
            		RoCo_initMoveSpeed_PARAM : -RoCo_initMoveSpeed_PARAM;
            angleDiffRequest = Ramp_out (&initRamp, rampTarget,
                                         RoCo_initRampSlopePos_PARAM,
                                         RoCo_initRampSlopeNeg_PARAM, dT);
            if (RoCo_legAngleValid) {
                init = FALSE;
                initBackwards = FALSE;
            }
        }

        if (!init) {
            if (RoCo_commandShutOff) {
                final = TRUE;
                RoCo_commandShutOff = FALSE;
                targetAngle = RoCo_idlePosition_PARAM;
                Timer_start (&shutdownTimer);
                RoCo_activeDesired = FALSE;
            }
            if (!final && RoCo_commandMoveToAngle) {
                targetAngle = RoCo_desiredTargetAngle;
                RoCo_commandMoveToAngle = FALSE;
                direction = 0;
            }
            else if (!final && RoCo_commandMoveByAngle) {
                targetAngle = RoCo_legAngle + RoCo_desiredDeltaAngle;
                if (RoCo_desiredDeltaAngle == 0.0) {
                    direction = 0;
                }
                else {
                    direction = (RoCo_desiredDeltaAngle > 0.0) ? -1 : 1;
                }
                RoCo_commandMoveByAngle = FALSE;
            }
            if (RoCo_hasMinMaxAngles_PARAM) {
                targetAngle = Limiter_out(RoCo_minAngle_PARAM, targetAngle,
                                          RoCo_maxAngle_PARAM);
            }
            t9 = RoCo_legAngle - targetAngle;
            t11 = RoCo_moveFast ? RoCo_angleReachedThreshold1Fast_PARAM :
                                  RoCo_angleReachedThreshold1_PARAM;
            if (RoCo_adaptationActive) {
            	t11 += adaptation[RoCo_moveFast];
            }
            if ((fabs(t9) > t11) && (direction == 0)) {
                direction = ((((!RoCo_hasMinMaxAngles_PARAM) ||
                               (fabs(t9) >= 180.0))
                              ? t9 : -t9) > 0.0) ? 1 : -1;
            }
            rampTarget = 0.0;
            if ((RoCo_legAngleValid) &&
                (fabs(t9) > t11) &&
                (((t9 > 0.0) && (0 > direction)) ||
                 ((t9 < 0.0) && (0 < direction)))) {
                rampTarget = 1.0;
            }
            rampValue = Ramp_out (&moveTimeRamp, rampTarget,
                    RoCo_TimeSlopePos_PARAM, RoCo_TimeSlopeNeg_PARAM, dT);
            if (-1 == Ramp_getDir (&moveTimeRamp)) {
                if (RoCo_moveFast) {
                    t12 = &RoCo_decelerationFast_CURVE;
                }
                else {
                    t12 = &RoCo_deceleration_CURVE;
                }
            }
            else {
                if (RoCo_moveFast) {
                    t12 = &RoCo_accelerationFast_CURVE;
                }
                else {
                    t12 = &RoCo_acceleration_CURVE;
                }
            }
            angleDiffRequest = Interpolate_from_curve (t12, rampValue);
            angleDiffRequest *= (real64)direction;
            t10 = RoCo_legAngle - targetAngle;
            if (fabs(rampValue) == 1.0) {
                rampHighReached = TRUE;
            }
            if (rampValue == 0.0) {
            	if (RoCo_adaptationActive && !wasRampEnded && rampHighReached) {
					if ((direction > 0 && t10 > 0.0) ||
						(direction < 0 && t10 < 0.0)) {
						adaptation[RoCo_moveFast] += fabs(t10);
						RoCo_isAdapted = TRUE;
					}
				}
            	if (fabs(t10) > RoCo_angleReachedThreshold2_PARAM) {
                    angleDiffRequest += ((t10 > 0.0) ? -1.0 : 1.0) *
                            RoCo_stepSpeed_PARAM * dT;
                }
            	rampHighReached = FALSE;
            }
            wasRampEnded = (rampValue == 0.0);
        }
        if (final &&
            (((RoCo_legAngleValid) &&
              (fabs(RoCo_legAngle - RoCo_idlePosition_PARAM) <
                    RoCo_angleReachedThreshold2_PARAM) &&
              (fabs(angleDiffRequest) < 0.05)) ||
             (Timer_elapsedTime (&shutdownTimer) > RoCo_shutdownTimeout_PARAM))) {
            final = FALSE;
            RoCo_isActive = FALSE;
            RoCo_legAngleValid = FALSE;
        }
        desiredEngineVoltage = Interpolate_from_curve
                (&EngineSpeedToVoltage_CURVE, angleDiffRequest);
        desiredEngineVoltage = Limiter_out (Engine_minVoltage_PARAM,
                desiredEngineVoltage, Engine_maxVoltage_PARAM);
        limitationActive = (Engine_maxVoltage_PARAM == desiredEngineVoltage) ||
                             (Engine_minVoltage_PARAM == desiredEngineVoltage);

	{ real64 tentativeV = PT1_Filter (&voltageFilter, desiredEngineVoltage,
					  t13, dT);
	  if (tentativeV - RoCo_engineVoltage > 0.390625)
	    RoCo_engineVoltage += 0.390625;
	  else if (tentativeV - RoCo_engineVoltage < -0.390625)
	    RoCo_engineVoltage -= 0.390625;
	  else 
	    RoCo_engineVoltage = tentativeV;
	}

        wasInit = init;
        wasActive = RoCo_isActive;
        Timer_tick (&shutdownTimer, dT);
        Timer_tick (&initTimer, dT);
    }
    else {
    	RoCo_engineVoltage = 0.0;
    }
}
