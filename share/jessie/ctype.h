/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2008                                               */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
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
/**************************************************************************/

/* $Id: ctype.h,v 1.1 2008/11/24 10:29:18 uid570 Exp $ */

#ifndef _CTYPE_H_
#define _CTYPE_H_

//@ assigns \nothing;
extern int isalnum(int c);

//@ assigns \nothing;
extern int isalpha(int c);
int isascii(int c);

//@ assigns \nothing;
extern int isblank(int c);
int iscntrl(int c);

//@ assigns \nothing;
extern int isdigit(int c);

//@ assigns \nothing;
extern int isgraph(int c);

//@ assigns \nothing;
extern int islower(int c);

//@ assigns \nothing;
extern int isprint(int c);

//@ assigns \nothing;
extern int ispunct(int c);

//@ assigns \nothing;
extern int isspace(int c);

//@ assigns \nothing;
extern int isupper(int c);

//@ assigns \nothing;
extern int isxdigit(int c);

#endif /* _CTYPE_H_ */
