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

/* POSIX header */

/* c_iflag bits */
#ifndef _TERMIOS_H
#define _TERMIOS_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_pid_t.h"

extern volatile int Frama_C_entropy_source;

#define IGNBRK	0000001
#define BRKINT	0000002
#define IGNPAR	0000004
#define PARMRK	0000010
#define INPCK	0000020
#define ISTRIP	0000040
#define INLCR	0000100
#define IGNCR	0000200
#define ICRNL	0000400
#define IUCLC	0001000
#define IXON	0002000
#define IXANY	0004000
#define IXOFF	0010000
#define IMAXBEL	0020000
#define IUTF8	0040000

/* c_oflag bits */
#define OPOST	0000001
#define OLCUC	0000002
#define ONLCR	0000004
#define OCRNL	0000010
#define ONOCR	0000020
#define ONLRET	0000040
#define OFILL	0000100
#define OFDEL	0000200

#define VTDLY	0040000
#define   VT0	0000000
#define   VT1	0040000

/* c_cflag bit meaning */
#define  B0	0000000		/* hang up */
#define  B50	0000001
#define  B75	0000002
#define  B110	0000003
#define  B134	0000004
#define  B150	0000005
#define  B200	0000006
#define  B300	0000007
#define  B600	0000010
#define  B1200	0000011
#define  B1800	0000012
#define  B2400	0000013
#define  B4800	0000014
#define  B9600	0000015
#define  B19200	0000016
#define  B38400	0000017
#define CSIZE	0000060
#define   CS5	0000000
#define   CS6	0000020
#define   CS7	0000040
#define   CS8	0000060
#define CSTOPB	0000100
#define CREAD	0000200
#define PARENB	0000400
#define PARODD	0001000
#define HUPCL	0002000
#define CLOCAL	0004000
#define  B57600   0010001
#define  B115200  0010002
#define  B230400  0010003
#define  B460800  0010004
#define  B500000  0010005
#define  B576000  0010006
#define  B921600  0010007
#define  B1000000 0010010
#define  B1152000 0010011
#define  B1500000 0010012
#define  B2000000 0010013
#define  B2500000 0010014
#define  B3000000 0010015
#define  B3500000 0010016
#define  B4000000 0010017
#define __MAX_BAUD B4000000
/* c_lflag bits */
#define ISIG	0000001
#define ICANON	0000002
#define ECHO	0000010
#define ECHOE	0000020
#define ECHOK	0000040
#define ECHONL	0000100
#define NOFLSH	0000200
#define TOSTOP	0000400
#define IEXTEN  0001000

/* tcflow() and TCXONC use these */
#define	TCOOFF		0
#define	TCOON		1
#define	TCIOFF		2
#define	TCION		3

/* tcflush() and TCFLSH use these */
#define	TCIFLUSH	0
#define	TCOFLUSH	1
#define	TCIOFLUSH	2

/* tcsetattr uses these */
#define	TCSANOW		0
#define	TCSADRAIN	1
#define	TCSAFLUSH	2

__BEGIN_DECLS

typedef unsigned int tcflag_t;
typedef unsigned char cc_t;
typedef unsigned int speed_t;

__END_DECLS

// cc_c characters
#define NCCS 32
#define VINTR 0
#define VQUIT 1
#define VERASE 2
#define VKILL 3
#define VEOF 4
#define VTIME 5
#define VMIN 6
#define VSWTC 7
#define VSTART 8
#define VSTOP 9
#define VSUSP 10
#define VEOL 11
#define VREPRINT 12
#define VDISCARD 13
#define VWERASE 14
#define VLNEXT 15
#define VEOL2 16

__BEGIN_DECLS

struct termios {
  tcflag_t c_iflag;    /* input specific flags (bitmask) */
  tcflag_t c_oflag;    /* output specific flags (bitmask) */
  tcflag_t c_cflag;    /* control flags (bitmask) */
  tcflag_t c_lflag;    /* local flags (bitmask) */
  cc_t     c_cc[NCCS]; /* special characters */
};

extern speed_t cfgetispeed(const struct termios *);
extern speed_t cfgetospeed(const struct termios *);
extern int     cfsetispeed(struct termios *, speed_t);
extern int     cfsetospeed(struct termios *, speed_t);
extern int     tcdrain(int);
extern int     tcflow(int, int);
extern int     tcflush(int, int);

/*@ requires valid_termios_p: \valid(termios_p);
    assigns \result, *termios_p \from indirect:fd,
                                       indirect:Frama_C_entropy_source;
    assigns Frama_C_entropy_source \from Frama_C_entropy_source;
    behavior ok:
      assumes nondet: Frama_C_entropy_source == 0; // arbitrary condition
      ensures initialization:termios_p: \initialized(termios_p);
      ensures result_ok: \result == 0;
    behavior error:
      assumes nondet: Frama_C_entropy_source != 0; // arbitrary condition
      ensures result_error: \result == -1;
    disjoint behaviors;
    complete behaviors;
 */
extern int     tcgetattr(int fd, struct termios *termios_p);

extern pid_t   tcgetsid(int);
extern int     tcsendbreak(int, int);

/*@
  requires valid_termios_p: \valid(termios_p);
  assigns *termios_p \from indirect:fd, indirect:optional_actions,
                      indirect:Frama_C_entropy_source, *termios_p;
  assigns Frama_C_entropy_source \from Frama_C_entropy_source;
  assigns \result \from indirect:fd, indirect:optional_actions,
                        indirect:Frama_C_entropy_source,
                        indirect:*termios_p;
  ensures result_ok_or_error: \result == 0 || \result == -1;
 */
extern int     tcsetattr(int fd, int optional_actions, struct termios *termios_p);

__END_DECLS

__POP_FC_STDLIB
#endif
