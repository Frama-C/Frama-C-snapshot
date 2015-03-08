/*
 * Public interface of the RoCo module.
 */

#ifndef ROCO_H_
#define ROCO_H_

/* --- functions --- */

/* Initialization, to be called once at start. */
void RoCo_init(void);

/* Cyclic function, to be called within the desired time raster. */
void RoCo_process(void);


/* --- configuration parameters --- */

/* specification of how engine voltage is to be increased or decreased */
extern Curve_t RoCo_acceleration_CURVE;
extern Curve_t RoCo_accelerationFast_CURVE;
extern Curve_t RoCo_deceleration_CURVE;
extern Curve_t RoCo_decelerationFast_CURVE;

/* angle at which the robot leg is when the sensor is passed */
extern real64 RoCo_angleAtMark_PARAM;

/* curve for transformation of differential leg sensor signal to an angle */
extern Curve_t LegSensorSignalToAngle_CURVE;

/* specification of when a target angle is considered to be reached */
extern real64 RoCo_angleReachedThreshold1_PARAM;
extern real64 RoCo_angleReachedThreshold2_PARAM;

/* parameters for battery voltage checks */
extern real64  RoCo_batteryLowLimit_PARAM;
extern real64  RoCo_batteryLowDelay_PARAM;
extern boolean RoCo_checkBatteryVoltage_PARAM;

/* parameters for humidity checks */
extern real64  RoCo_humidityLimit_PARAM;
extern boolean RoCo_checkHumidity_PARAM;

/* parameters for environment temperature checks */
extern real64 RoCo_envTempLowerLimit_PARAM;
extern real64 RoCo_envTempUpperLimit_PARAM;
extern real64 RoCo_TempFltT_PARAM;

/* does the robot leg have min/max angles, or can it turn around 360 degrees? */
extern boolean RoCo_hasMinMaxAngles_PARAM;
/* for systems with min/max angles: the min/max angle values */
extern real64 RoCo_maxAngle_PARAM;
extern real64 RoCo_minAngle_PARAM;

/* robot leg movement speed and ramps up/down during initialization */
extern real64 RoCo_initMoveSpeed_PARAM;
extern real64 RoCo_initRampSlopeNeg_PARAM;
extern real64 RoCo_initRampSlopePos_PARAM;

/* position at which the robot leg should be parked when inactive */
extern real64 RoCo_idlePosition_PARAM;

/* maximum time for finding the sensor position during initialization */
extern real64 RoCo_initTimeout_PARAM;

/* timeout for shutdown sequence */
extern real64 RoCo_shutdownTimeout_PARAM;

/* engine voltage during stepping */
extern real64 RoCo_stepSpeed_PARAM;

/* engine ramp slopes */
extern real64 RoCo_TimeSlopeNeg_PARAM;
extern real64 RoCo_TimeSlopePos_PARAM;

/* curve for conversion of speed request to engine voltage output */
extern Curve_t EngineSpeedToVoltage_CURVE;

/* min/max voltage that may be applied to the engine */
extern real64 Engine_maxVoltage_PARAM;
extern real64 Engine_minVoltage_PARAM;
extern real64 RoCo_voltageFilter_PARAM;

/* --- control messages --- */

/* activate robot leg controller */
extern boolean RoCo_activeDesired;

/* move robot leg by a certain angle */
extern boolean RoCo_commandMoveByAngle;
extern real64  RoCo_desiredDeltaAngle;

/* move robot leg to a certain position */
extern boolean RoCo_commandMoveToAngle;
extern real64  RoCo_desiredTargetAngle;

/* for movement requests: is fast movement requested? */
extern boolean RoCo_moveFast;

/* shut down robot leg controller */
extern boolean RoCo_commandShutOff;

/* --- result messages --- */

/* if TRUE, a fatal error has occurred and the controller has stopped working */
extern boolean RoCo_error;

/* if TRUE, the controller is actively controlling the leg */
extern boolean RoCo_isActive;

/* the current position of the robot leg; only valid if legAngleValid is TRUE. */
extern real64  RoCo_legAngle;
extern boolean RoCo_legAngleValid;

/* the voltage that should be applied to the engine (main output value) */
extern real64 RoCo_engineVoltage;

#endif /* ROCO_H_ */
