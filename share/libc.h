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

#ifndef FRAMA_C_LIBC_H_
#define FRAMA_C_LIBC_H_

#include "machine.h"

void* memcpy(void* region1, const void* region2, size_t n);

void* memset (void* dest, int val, size_t len);

int strcmp(const char *s1, const char *s2);

char* strcat(char *s1, const char *s2);

char* strcpy(char *s1, const char *s2);

char* strncpy(char *s1, const char *s2, size_t n);

int strncmp(const char *s1, const char *s2, size_t n);

size_t strlen(const char *s);

int memcmp(const void *s1, const void *s2, size_t n);

int atoi(const char *p);

#endif
