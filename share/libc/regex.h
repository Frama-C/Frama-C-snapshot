/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2014                                               */
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

#ifndef _REGEX_H
#define _REGEX_H 1
#include "__fc_define_restrict.h"
#include "__fc_define_size_t.h"

struct re_pattern_buffer { size_t re_nsub;  };

typedef struct re_pattern_buffer regex_t;

#define REG_EXTENDED 1
#define REG_ICASE 2
#define REG_NEWLINE 4
#define REG_NOSUB 8


/* Eflags */
#define REG_NOTBOL 1
#define REG_NOTEOL 2

/* Error codes */
typedef enum
{
  REG_NOERROR = 0,
  REG_NOMATCH,	  
  REG_BADPAT,	  
  REG_ECOLLATE,	  
  REG_ECTYPE,	  
  REG_EESCAPE,	  
  REG_ESUBREG,	  
  REG_EBRACK,	  
  REG_EPAREN,	  
  REG_EBRACE,	  
  REG_BADBR,	  
  REG_ERANGE,	  
  REG_ESPACE,	  
  REG_BADRPT,	  
  REG_EEND,	  
  REG_ESIZE,	  
  REG_ERPAREN	  
} reg_errcode_t;

typedef int regoff_t;

typedef struct
{
  regoff_t rm_so;
  regoff_t rm_eo;
} regmatch_t;

int    regcomp(regex_t *, const char *, int);
int    regexec(const regex_t *, const char *, size_t, regmatch_t[], int);
size_t regerror(int, const regex_t *, char *, size_t);
void   regfree(regex_t *);

#endif
