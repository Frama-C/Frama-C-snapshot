/*
 * Test driver to demonstrate the RoCo module in collaboration
 * with the robot arm simulator.
 */

#include "stdafx.h"

#include "roco.h"
#include "sim.h"

#ifndef TESTCASE

char *output_header = "time(ms)\tengineVoltage\tlegAngle\tlegAngleValid\tisActive\n";
char *output_row_fmt_string = "%d\t%f\t%f\t%d\t%d\n";

int main()

{
	sint32 lastTime = Time();
	dT = 0.02;
	RoCo_init();
	Sim_init();

	Battery_voltage = 21.3;
	Env_humidity = 0.5;
	Env_temperature = 20.0;
	Global_emergencyMode = FALSE;

	RoCo_activeDesired = TRUE;
	RoCo_moveFast = FALSE;
	int cnt = 0;

	fprintf(stderr, output_header);

	while (cnt < 9000) {
		dT = (Time() - lastTime) / 1000.0;
		lastTime = Time();
		RoCo_process();
		Sim_process();
		if (cnt % 5 == 0) {
			fprintf(stderr, output_row_fmt_string, lastTime, RoCo_engineVoltage,
					RoCo_legAngle, RoCo_legAngleValid, RoCo_isActive);
		}
		Sleep(20); /* milliseconds */
		cnt ++;

		if (cnt==2500) {
			RoCo_commandMoveToAngle = TRUE;
			RoCo_desiredTargetAngle = 15.0;
		}
		else if (cnt==3500) {
			RoCo_commandMoveToAngle = TRUE;
			RoCo_desiredTargetAngle = -55.0;
		}
		else if (cnt==5000) {
			RoCo_commandMoveToAngle = TRUE;
			RoCo_desiredTargetAngle = 30.0;
		}
		else if (cnt==7000) {
			RoCo_commandMoveToAngle = TRUE;
			RoCo_desiredTargetAngle = -5.0;
		}
		else if (cnt==8000) {
			RoCo_commandShutOff = TRUE;
		}
	}
	return 0;
}

#endif /* TESTCASE */
