/*
 * Standard implementations of some general-purpose library functions.
 * See external.h for interface documentation.
 */

#include "stdafx.h" /* Must be first line of code in the file */

/* -- external messages -- */
real64  Battery_voltage;
real64  dT;
real64  Engine_realVoltage;
real64  Env_humidity;
real64  Env_temperature;
boolean Global_emergencyMode;
boolean LegSensor_atMark;
real64  LegSensor_signal;


/* ------------------------ INTERPOLATION AND FILTERS ------------------------- */

real64 PT1_Filter(PT1_t *state, real64 x, real64 t1, real64 dt) {
    if (t1 == 0.0) {
        *state = x;
    }
    else {
    	real64 c = exp(-dt/t1);
        *state = (1.0 - c) * x + c * (*state);
    }
    return *state;
}


real64 Interpolate_from_curve(Curve_t *curve, real64 x) {
    real64 result = 0.0;
    sint16 i;

    if( x <= curve->x[0]) {
        result = curve->y[0];
    }
    else
    {
        if (x >= curve->x[curve->numPoints-1]) {
            result = curve->y[curve->numPoints-1];
        }
        else {
        	for (i=curve->numPoints-2; i>=0; i--) {
        		if (x >= curve->x[i]) {
        			result = curve->y[i] +
         					 (x - curve->x[i]) / (curve->x[i+1] - curve->x[i]) *
         					 (curve->y[i+1] - curve->y[i]);
        			break;
        		}
        	}
        }
    }
    return result;
}

/* ------------------------------- TIMERS AND DELAY -------------------------- */

boolean Turn_on_delay(Delay_t *delay, boolean signal, real64 timeLimit, real64 dt) {
	boolean result;
	if (signal) {
		if (*delay < timeLimit) {
			*delay += dt;
			result = FALSE;
		}
		else {
			result = TRUE;
		}
	}
	else {
		*delay = 0.0;
		result = FALSE;
	}
	return result;
}


void Timer_start(Timer_t *timer) {
	*timer = 0.0;
}

real64 Timer_elapsedTime(Timer_t *timer) {
	return *timer;
}

void Timer_tick(Timer_t *timer, real64 dt) {
	*timer += dt;
}

static sint32 _t;

void Sleep(sint32 delayMs) {
	/* system-specific implementation */
	_t += delayMs;
}

sint32 Time() {
	/* system-specific implementation */
	return _t;
}

/* ----------------------------------- RAMP ------------------------------------ */

real64 Ramp_out(Ramp_t *data, real64 target, real64 slopePos, real64 slopeNeg,
			    real64 dt) {
	data->target = target;
	if ((data->state < target) && (slopePos != 0.0)) {
		if (data->dir == -1) {
			data->state = target;
		}
		else {
			data->state += slopePos * dt;
			if (data->state > target) {
				data->state = target;
			}
			data->dir = 1;
		}
	}
	else if ((data->state > target) && (slopeNeg != 0.0)) {
		if (data->dir == 1) {
			data->state = target;
		}
		else {
			data->state -= slopeNeg * dt;
			if (data->state < target) {
				data->state = target;
			}
			data->dir = -1;
		}
	}
	if (data->state == target) {
		data->dir = 0;
	}
	return data->state;
}

sint16 Ramp_getDir(Ramp_t *data) {
	return data->dir;
}

real64 Ramp_getValue(Ramp_t *data) {
	return data->state;
}

boolean Ramp_targetReached(Ramp_t *data) {
	return data->state == data->target;
}

/* -------------------------------- LIMITER ------------------------------- */

real64 Limiter_out (real64 min, real64 input, real64 max) {
	real64 result = input;
	if (result < min) {
		result = min;
	}
	if (result > max) {
		result = max;
	}
	return result;
}
