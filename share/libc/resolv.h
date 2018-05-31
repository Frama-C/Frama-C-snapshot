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

#ifndef _RESOLV_H
#define _RESOLV_H 1
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

// Note: resolv.h is neither ISO-C nor POSIX

struct __res_state;
typedef struct __res_state *res_state;

// deprecated
extern int res_init(void);

extern int res_ninit(res_state statep);

extern int res_nquery(res_state statep,
                      const char *dname, int class, int type,
                      unsigned char *answer, int anslen);

extern int res_nsearch(res_state statep,
                       const char *dname, int class, int type,
                       unsigned char *answer, int anslen);

extern int res_nquerydomain(res_state statep,
                            const char *name, const char *domain,
                            int class, int type, unsigned char *answer,
                            int anslen);

extern int res_nmkquery(res_state statep,
                        int op, const char *dname, int class,
                        int type, const unsigned char *data, int datalen,
                        const unsigned char *newrr,
                        unsigned char *buf, int buflen);

extern int res_nsend(res_state statep,
                     const unsigned char *msg, int msglen,
                     unsigned char *answer, int anslen);

extern int dn_comp(const char *exp_dn, unsigned char *comp_dn,
                   int length, unsigned char **dnptrs,
                   unsigned char **lastdnptr);

extern int dn_expand(const unsigned char *msg,
                     const unsigned char *eomorig,
                     const unsigned char *comp_dn, char *exp_dn,
                     int length);

__END_DECLS

__POP_FC_STDLIB
#endif
