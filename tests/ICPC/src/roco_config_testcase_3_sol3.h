/*
 * Configuration parameters and concrete configuration.
 * Adjust these values to meet individual needs.
 */

Curve_t RoCo_acceleration_CURVE =
        { 4, {0.0, 0.333, 0.667, 1.0}, {0.0, 0.4, 0.7, 1.0} };
Curve_t RoCo_accelerationFast_CURVE =
        { 5, {0.0, 0.333, 0.5, 0.667, 1.0}, {0.0, 4.0, 6.0, 8.0, 10.0} };
boolean RoCo_adaptationActive = TRUE;
real64 RoCo_angleAtMark_PARAM = 11.5;
real64 RoCo_angleReachedThreshold1_PARAM = 7.5;
real64 RoCo_angleReachedThreshold1Fast_PARAM = 15.0;
real64 RoCo_angleReachedThreshold2_PARAM = 0.5;
real64 RoCo_batteryLowLimit_PARAM = 11.5;
real64 RoCo_batteryLowDelay_PARAM = 120.0;
boolean RoCo_checkBatteryVoltage_PARAM = TRUE;
boolean RoCo_checkHumidity_PARAM = TRUE;
Curve_t RoCo_deceleration_CURVE =
        { 4, {0.0, 0.33, 0.67, 1.0}, {0.0, 0.3, 0.6, 1.0} };
Curve_t RoCo_decelerationFast_CURVE =
        {5, {0.0, 0.33, 0.5, 0.67, 1.0}, {0.0, 4.0, 6.0, 8.0, 10.0} };
real64 RoCo_envTempLowerLimit_PARAM = -10.0;
real64 RoCo_envTempUpperLimit_PARAM = 40.0;
boolean RoCo_hasMinMaxAngles_PARAM = TRUE;
real64 RoCo_humidityLimit_PARAM = 0.9;
real64 RoCo_idlePosition_PARAM = 0.0;
real64 RoCo_initMoveSpeed_PARAM = 5.0;
real64 RoCo_initRampSlopeNeg_PARAM = 1.0;
real64 RoCo_initRampSlopePos_PARAM = 1.0;
real64 RoCo_initStandstillTimeout_PARAM = 5.0;
real64 RoCo_initTimeout_PARAM = 30.0;
real64 RoCo_initialDeltaSearchRange_PARAM = 3.0;
real64 RoCo_initialDeltaSearchTimeout_PARAM = 3.0;
real64 RoCo_maxAngle_PARAM = 90.0;
real64 RoCo_minAngle_PARAM = -90.0;
real64 RoCo_shutdownTimeout_PARAM = 12.0;
real64 RoCo_stepSpeed_PARAM = 8.0;
real64 RoCo_TempFltT_PARAM = 0.15;
real64 RoCo_TimeSlopeNeg_PARAM = 0.5;
real64 RoCo_TimeSlopePos_PARAM = 0.5;
real64 RoCo_voltageFilter_PARAM = 0.1;

real64 Engine_maxVoltage_PARAM = 15.0;
real64 Engine_minVoltage_PARAM = -15.0;
Curve_t EngineSpeedToVoltage_CURVE =
        { 5, {-3.0, -1.0, 0.0, 1.0, 3.0}, {-15.0, -7.0, 0.0, 7.0, 15.0} };;
Curve_t LegSensorSignalToAngle_CURVE =
        { 4, {-24.0, -12.0, 12.0, 24.0}, {-60.0, -30.0, 30.0, 60.0} };
