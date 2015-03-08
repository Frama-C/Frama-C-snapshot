/*
 * Global definitions and library functions.
 */

#ifndef extern_H_
#define extern_H_

/* BASIC TYPES */
typedef unsigned long 		 uint64;
typedef unsigned int   		 uint32;
typedef int 		   		 sint32;
typedef short 		   		 sint16;
typedef unsigned char  		 uint8;
typedef unsigned short 		 uint16;
typedef char 		   		 boolean;
typedef double               real64;

/* BASIC MACROS */
#define FALSE 				 0
#define TRUE 				 1


extern real64 Engine_maxVoltage_PARAM;
extern real64 Engine_minVoltage_PARAM;


/* -- external messages -- */
extern real64  Battery_voltage;
extern real64  dT;
extern real64  Engine_realVoltage;
extern real64  Env_humidity;
extern real64  Env_temperature;
extern boolean Global_emergencyMode;
extern boolean LegSensor_atMark;
extern real64  LegSensor_signal;


/* ---------------------------- FILTERS ------------------ */

typedef real64 PT1_t;

/* Proportional element with 1st order time delay (Kp=1).

     x      Input value for the PT1 element
     state  Internal state of the PT1 element
     t1Rec  Time factor as reciprocal value.
     dt     Sample time.

   Returns the PT1 filtered response to the input signal.
*/
real64 PT1_Filter(PT1_t *state, real64 x, real64 t1, real64 Dt);

/* set the state of the filter */
#define PT1_SetState(STATE, VAL)  ( (*(STATE)) = (VAL) )


/* ------------------------- INTERPOLATION ------------------------ */

#define MAX_CURVE_POINTS 10

typedef struct {
	uint16 numPoints;
	real64 x[MAX_CURVE_POINTS];
	real64 y[MAX_CURVE_POINTS];
} Curve_t;

/* Interpolates the curve at position X by calculating:

                   (y1-y0) * (X-x0)
    result = y0 + ------------------
                       (x1-x0)
   where:
    x0 = position before X
    x1 = position after X
    y0 = value at x0
    y1 = value at x1
*/
real64 Interpolate_from_curve(Curve_t *curve, real64 x);

/* --------------------------------- DELAY ------------------------- */

typedef real64 Delay_t;

/* When signal turns from FALSE to TRUE, this is delayed by the function
   by timeLimit seconds.

   signal    = incoming signal
   timeLimit = delay time
   dT        = time since last call (time raster)

   Return value is the delayed logical value.
 */
boolean Turn_on_delay(Delay_t *delay, boolean signal, real64 timeLimit, real64 dT);

/* --------------------------------- TIMER ------------------------- */

typedef real64 Timer_t;

/* Start the timer at zero. */
void Timer_start(Timer_t *timer);

/* Get the time that has elapsed since the timer has been started. */
real64 Timer_elapsedTime(Timer_t *timer);

/* Advance the timer by the specified time dT. */
void Timer_tick(Timer_t *timer, real64 dT);

/* ------------------------------- RAMP ----------------------------- */

typedef struct {
	sint16 dir;
	real64 state;
	real64 target;
} Ramp_t;

/* A ramp performs a linear transition from the current (internal) value to the
   target value. The slope of the ramp can be specified as a parameter.
   This function calculates the new ramp value and returns it.

   target   = target value of the ramp
   slopePos = positive slope (i.e. when ramp is rising)
   slopeNeg = negative slope (i.e. when ramp is falling)
   dT       = time since last call, i.e. time raster

   Returns the current ramp value.
 */
real64 Ramp_out(Ramp_t *data, real64 target, real64 slopePos, real64 slopeNeg,
		        real64 dT);

/* Returns the current direction of the ramp:
   -1 = falling, 0 = constant (target reached), 1 = rising
 */
sint16 Ramp_getDir(Ramp_t *data);

/* Returns the current value of the ramp (without calculation). */
real64 Ramp_getValue(Ramp_t *data);

/* Returns TRUE when the target value has been reached. */
boolean Ramp_targetReached(Ramp_t *data);

/* ------------------------------------------ LIMITER ------------------------- */

/* Limits a value by an interval (min, max).
   The returned value is guaranteed to lie within the given interval.
 */
real64 Limiter_out (real64 min, real64 input, real64 max);

/* ---------------------------------- INTERNAL TIMER --------------------------- */

/* stops processing for delayMs milliseconds */
void Sleep(sint32 delayMs);

/* gets the current system time */
sint32 Time();

#endif /* extern_H_ */
