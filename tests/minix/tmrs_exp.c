/*License

Copyright (c) 1987,1997, 2006, Vrije Universiteit, Amsterdam, The Netherlands All rights reserved. Redistribution and use of the MINIX 3 operating system in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    * Neither the name of the Vrije Universiteit nor the names of the software authors or contributors may be used to endorse or promote products derived from this software without specific prior written permission.
    * Any deviations from these conditions require written permission from the copyright holder in advance


Disclaimer

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS, AUTHORS, AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR ANY AUTHORS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*/
#define LONG_MAX  2147483647L   /* maximum value of a long */
#define NULL    ((void *)0)

typedef void (*tmr_func_t)(struct timer *tp);
typedef union { int ta_int; long ta_long; void *ta_ptr; } tmr_arg_t;
typedef long clock_t;              /* unit for system accounting */

typedef struct timer
{
  struct timer  *tmr_next;      /* next in a timer chain */
  clock_t       tmr_exp_time;   /* expiration time */
  tmr_func_t    tmr_func;       /* function to call when expired */
  tmr_arg_t     tmr_arg;        /* random argument */
} timer_t;


/* Used when the timer is not active. */
#define TMR_NEVER    ((clock_t) -1 < 0) ? ((clock_t) LONG_MAX) : ((clock_t) -1)
#undef TMR_NEVER
#define TMR_NEVER       ((clock_t) LONG_MAX)

void tmrs_exptimers (timer_t **tmrs, clock_t now, clock_t *new_head);

/*===========================================================================*
 *				tmrs_exptimers				     *
 *===========================================================================*/
//@ predicate ptime_var(clock_t * time) = \valid(time) && *time >= 0 && *time <= 2147483647 ;
//@ predicate time_var(clock_t time) = time >= 0 && time <= 2147483647;

/*@ requires \valid(tmrs) && ptime_var(new_head) && time_var(now);
  @ ensures \valid(new_head);
  @*/
void tmrs_exptimers(tmrs, now, new_head)
timer_t **tmrs;				/* pointer to timers queue */
clock_t now;				/* current time */
clock_t *new_head;
{
/* Use the current time to check the timers queue list for expired timers.
 * Run the watchdog functions for all expired timers and deactivate them.
 * The caller is responsible for scheduling a new alarm if needed.
 */

  timer_t *tp;

  //@ loop invariant tp->tmr_exp_time <= now ;
  while ((tp = *tmrs) != NULL && tp->tmr_exp_time <= now) {
	*tmrs = tp->tmr_next;
	tp->tmr_exp_time = TMR_NEVER;
	(*tp->tmr_func)(tp);
  }
  //@ assert tp->tmr_exp_time > now;

  if(new_head) {
  	if(*tmrs)
  		*new_head = (*tmrs)->tmr_exp_time;
  	else
  		*new_head = 0;
  }
}

void f(struct timer *tp) {
  int i = (tp->tmr_arg).ta_int;
  i += 5;
}

int main(clock_t realtime, timer_t tp, timer_t *clock_timers ) {
 tp.tmr_next = NULL;
 tp.tmr_exp_time = 10;
 tp.tmr_func = f;
 (tp.tmr_arg).ta_int = 5;

 tmrs_exptimers(&clock_timers, realtime, NULL);
 return 0;
}
