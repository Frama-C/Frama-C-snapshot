#include "stdafx.h"

#include "roco.h"
#include "sim.h"

real64 buf1;
real64 buf2;
real64 buf3;
real64 buf4;
real64 buf5;
real64 internalPosition = 22.5;
real64 oldInternalPosition;
real64 Sim_maxPos_PARAM = 100.0;
real64 Sim_minPos_PARAM = -100.0;
PT1_t Srv_PT1;

extern real64 RoCo_angleAtMark_PARAM;
extern real64 RoCo_minAngle_PARAM;
extern real64 RoCo_maxAngle_PARAM;

void Sim_process()
{
	real64 t1 = RoCo_engineVoltage;
	real64 t2 = Engine_realVoltage;
	uint32 t3 = LegSensor_atMark;
	real64 t4 = LegSensor_signal;
	buf5 = buf4;
	buf4 = buf3;
	buf3 = buf2;
	buf2 = buf1;
	buf1 = t1;
	Sim_maxPos_PARAM = RoCo_maxAngle_PARAM + 1.0;
	Sim_minPos_PARAM = RoCo_minAngle_PARAM - 1.0;
	internalPosition = Limiter_out (internalPosition +
			(PT1_Filter (&Srv_PT1, buf5 * 0.9, 0.15, dT) * dT),
			Sim_minPos_PARAM, Sim_maxPos_PARAM);
	t4 = internalPosition - oldInternalPosition;
	t3 = (uint8)(((oldInternalPosition < RoCo_angleAtMark_PARAM) &&
			      (RoCo_angleAtMark_PARAM <= internalPosition)) ||
			     ((oldInternalPosition > RoCo_angleAtMark_PARAM) &&
			      (RoCo_angleAtMark_PARAM >= internalPosition)));
	t2 = buf5 * 0.9;
	oldInternalPosition = internalPosition;
	Engine_realVoltage = t2;
	LegSensor_atMark = t3;
	LegSensor_signal = t4 / 2.5;
}

void Sim_init()
{
	Engine_realVoltage = 0.0;
	LegSensor_atMark = FALSE;
	LegSensor_signal = 0.0;
	buf1 = 0.0;
	buf2 = 0.0;
	buf3 = 0.0;
	buf4 = 0.0;
	buf5 = 0.0;
	internalPosition = 20.0;
	oldInternalPosition = 0.0;
	Srv_PT1 = 0.0;
}
