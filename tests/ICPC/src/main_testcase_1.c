/*
 * Test driver to demonstrate the RoCo module in collaboration
 * with the robot arm simulator.
 */

#include "stdafx.h" /* Must be first line of code in the file */

#include "roco.h"
#include "sim.h"

#if TESTCASE == 1

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

	while (cnt < 10000) {
		dT = (Time() - lastTime) / 1000.0;
		lastTime = Time();
		RoCo_process();
		Sim_process();
		if (cnt % 5 == 0) {
			fprintf(stderr, output_row_fmt_string, lastTime, RoCo_engineVoltage,
					RoCo_legAngle, RoCo_legAngleValid, RoCo_isActive);
			/* BM: the bug is detected at time 13400 by the script*/
			// @ assert lastTime != 13400 ;
		}
		Sleep(20); /* milliseconds */
		cnt ++;

		if (cnt==1500) {
			RoCo_commandMoveToAngle = TRUE;
			RoCo_desiredTargetAngle = 25.0;
		}
		else if (cnt==2500) {
			RoCo_commandMoveByAngle = TRUE;
			RoCo_desiredDeltaAngle = -70.0;
		}
		else if (cnt==8000) {
			RoCo_commandMoveToAngle = TRUE;
			RoCo_desiredTargetAngle = -20.0;
		}
		else if (cnt==9000) {
			RoCo_commandShutOff = TRUE;
		}
	}
	return 0;
}

#endif
