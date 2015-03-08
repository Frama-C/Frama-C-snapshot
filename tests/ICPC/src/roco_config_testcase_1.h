/*
 * Configuration parameters and concrete configuration.
 * Adjust these values to meet individual needs.
 */

Curve_t RoCo_acceleration_CURVE =
        { 4, {0.0, 0.3, 0.6, 1.0}, {0.0, 0.25, 0.5, 1.0} };
Curve_t RoCo_accelerationFast_CURVE =
        { 4, {0.0, 0.4, 0.75, 1.0}, {0.0, 2.5, 4.5, 6.0} };
boolean RoCo_adaptationActive = TRUE;
real64 RoCo_angleAtMark_PARAM = -5.0;
real64 RoCo_angleReachedThreshold1_PARAM = 6.0;
real64 RoCo_angleReachedThreshold1Fast_PARAM = 12.0;
real64 RoCo_angleReachedThreshold2_PARAM = 0.4;
real64 RoCo_batteryLowLimit_PARAM = 12.0;
real64 RoCo_batteryLowDelay_PARAM = 300.0;
boolean RoCo_checkBatteryVoltage_PARAM = TRUE;
boolean RoCo_checkHumidity_PARAM = FALSE;
Curve_t RoCo_deceleration_CURVE =
        { 4, {0.0, 0.33, 0.67, 1.0}, {0.0, 0.33, 0.67, 1.0} };
Curve_t RoCo_decelerationFast_CURVE =
        {4, {0.0, 0.33, 0.67, 1.0}, {0.0, 2.0, 4.0, 6.0} };
real64 RoCo_envTempLowerLimit_PARAM = -10.0;
real64 RoCo_envTempUpperLimit_PARAM = 50.0;
boolean RoCo_hasMinMaxAngles_PARAM = TRUE;
real64 RoCo_humidityLimit_PARAM = 0.95;
real64 RoCo_idlePosition_PARAM = 0.0;
real64 RoCo_initMoveSpeed_PARAM = 1.2;
real64 RoCo_initRampSlopeNeg_PARAM = 1.0;
real64 RoCo_initRampSlopePos_PARAM = 1.0;
real64 RoCo_initStandstillTimeout_PARAM = 5.0;
real64 RoCo_initTimeout_PARAM = 20.0;
real64 RoCo_initialDeltaSearchRange_PARAM = 5.0;
real64 RoCo_initialDeltaSearchTimeout_PARAM = 5.0;
real64 RoCo_maxAngle_PARAM = 180.0;
real64 RoCo_minAngle_PARAM = -90.0;
real64 RoCo_shutdownTimeout_PARAM = 30.0;
real64 RoCo_stepSpeed_PARAM = 5.0;
real64 RoCo_TempFltT_PARAM = 0.1;
real64 RoCo_TimeSlopeNeg_PARAM = 1.0;
real64 RoCo_TimeSlopePos_PARAM = 0.5;
real64 RoCo_voltageFilter_PARAM = 0.15;

real64 Engine_maxVoltage_PARAM = 18.0;
real64 Engine_minVoltage_PARAM = -18.0;
Curve_t EngineSpeedToVoltage_CURVE =
        { 5, {-3.0, -1.5, 0.0, 1.5, 3.0}, {-18.0, -12.0, 0.0, 12.0, 18.0} };;
Curve_t LegSensorSignalToAngle_CURVE =
        { 4, {-24.0, -12.0, 12.0, 24.0}, {-60.0, -35.0, 35.0, 60.0} };
