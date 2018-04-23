/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2018                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef _SYS_TIMEX_H
#define _SYS_TIMEX_H 1
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

#define ADJ_OFFSET 0x0001
#define ADJ_FREQUENCY 0x0002
#define ADJ_MAXERROR 0x0004
#define ADJ_ESTERROR 0x0008
#define ADJ_STATUS 0x0010
#define ADJ_TIMECONST 0x0020
#define ADJ_TICK 0x4000
#define ADJ_OFFSET_SINGLESHOT 0x8001

#define MOD_OFFSET ADJ_OFFSET
#define MOD_FREQUENCY ADJ_FREQUENCY
#define MOD_MAXERROR ADJ_MAXERROR
#define MOD_ESTERROR ADJ_ESTERROR
#define MOD_STATUS ADJ_STATUS
#define MOD_TIMECONST ADJ_TIMECONST
#define MOD_CLKB ADJ_TICK
#define MOD_CLKA ADJ_OFFSET_SINGLESHOT

#define STA_PLL 0x0001
#define STA_PPSFREQ 0x0002
#define STA_PPSTIME 0x0004
#define STA_FLL 0x0008

#define STA_INS 0x0010
#define STA_DEL 0x0020
#define STA_UNSYNC 0x0040
#define STA_FREQHOLD 0x0080

#define STA_PPSSIGNAL 0x0100
#define STA_PPSJITTER 0x0200
#define STA_PPSWANDER 0x0400
#define STA_PPSERROR 0x0800

#define STA_CLOCKERR 0x1000

#define STA_RONLY (STA_PPSSIGNAL | STA_PPSJITTER | STA_PPSWANDER | \
    STA_PPSERROR | STA_CLOCKERR)

#define TIME_OK 0
#define TIME_INS 1
#define TIME_DEL 2
#define TIME_OOP 3
#define TIME_WAIT 4
#define TIME_ERROR 5
#define TIME_BAD TIME_ERROR

struct timex {
  unsigned int modes;
  int64_t offset;
  int64_t freq;
  int64_t maxerror;
  int64_t esterror;
  int status;
  int64_t constant;
  int64_t precision;
  int64_t tolerance;
  struct timeval time;
  int64_t tick;

  int64_t ppsfreq;
  int64_t jitter;
  int shift;
  int64_t stabil;
  int64_t jitcnt;
  int64_t calcnt;
  int64_t errcnt;
  int64_t stbcnt;
  int tai;

  int32_t _padding[11];
};

extern int adjtimex(struct timex *buf);
extern int ntp_adjtime(struct timex *buf);

__END_DECLS

__POP_FC_STDLIB
#endif
