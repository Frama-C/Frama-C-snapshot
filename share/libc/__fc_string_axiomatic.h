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

/* $Id: jessie_machine_prolog.h,v 1.8 2008-12-09 10:17:25 uid525 Exp $ */

#ifndef __FC_STRING_AXIOMATIC
#define __FC_STRING_AXIOMATIC

#include "__fc_define_null.h"
#include "__fc_define_wchar_t.h"

/*@ axiomatic MemCmp {
  @ logic ℤ memcmp{L}(char *s1, char *s2, ℤ n)
  @   reads s1[0..n - 1], s2[0..n - 1];
  @
  @ axiom memcmp_zero{L}:
  @   \forall char *s1, *s2; \forall ℤ n;
  @      memcmp(s1,s2,n) == 0
  @      <==> \forall ℤ i; 0 <= i < n ==> s1[i] == s2[i];
  @
  @ }
  @*/


/*@ axiomatic MemChr {
  @ logic 𝔹 memchr{L}(char *s, ℤ c, ℤ n);
  @ // reads s[0..n - 1];
  @ // Returns [true] iff array [s] contains character [c]
  @
  @ axiom memchr_def{L}:
  @   \forall char *s; \forall ℤ c; \forall ℤ n;
  @      memchr(s,c,n) <==> \exists int i; 0 <= i < n && s[i] == c;
  @ }
  @*/

/*@ axiomatic MemSet {
  @ logic 𝔹 memset{L}(char *s, ℤ c, ℤ n);
  @ // reads s[0..n - 1];
  @ // Returns [true] iff array [s] contains only character [c]
  @
  @ axiom memset_def{L}:
  @   \forall char *s; \forall ℤ c; \forall ℤ n;
  @      memset(s,c,n) <==> \forall ℤ i; 0 <= i < n ==> s[i] == c;
  @ }
  @*/

/*@ axiomatic StrLen {
  @ logic ℤ strlen{L}(char *s);
  @ // reads s[0..];
  @
  @ axiom strlen_pos_or_null{L}:
  @   \forall char* s; \forall ℤ i;
  @      (0 <= i
  @       && (\forall ℤ j; 0 <= j < i ==> s[j] != '\0')
  @       && s[i] == '\0') ==> strlen(s) == i;
  @
  @ axiom strlen_neg{L}:
  @   \forall char* s;
  @      (\forall ℤ i; 0 <= i ==> s[i] != '\0')
  @      ==> strlen(s) < 0;
  @
  @ axiom strlen_before_null{L}:
  @   \forall char* s; \forall ℤ i; 0 <= i < strlen(s) ==> s[i] != '\0';
  @
  @ axiom strlen_at_null{L}:
  @   \forall char* s; 0 <= strlen(s) ==> s[strlen(s)] == '\0';
  @
  @ axiom strlen_not_zero{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i <= strlen(s) && s[i] != '\0' ==> i < strlen(s);
  @
  @ axiom strlen_zero{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i <= strlen(s) && s[i] == '\0' ==> i == strlen(s);
  @
  @ axiom strlen_sup{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i && s[i] == '\0' ==> 0 <= strlen(s) <= i;
  @
  @ axiom strlen_shift{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i <= strlen(s) ==> strlen(s + i) == strlen(s) - i;
  @
  @ axiom strlen_create{L}:
  @   \forall char* s; \forall ℤ i;
  @      0 <= i && s[i] == '\0' ==> 0 <= strlen(s) <= i;
  @
  @ axiom strlen_create_shift{L}:
  @   \forall char* s; \forall ℤ i; \forall ℤ k;
  @      0 <= k <= i && s[i] == '\0' ==> 0 <= strlen(s+k) <= i - k;
  @
  @ axiom memcmp_strlen_left{L}:
  @   \forall char *s1, *s2; \forall ℤ n;
  @      memcmp(s1,s2,n) == 0 && strlen(s1) < n ==> strlen(s1) == strlen(s2);
  @
  @ axiom memcmp_strlen_right{L}:
  @   \forall char *s1, *s2; \forall ℤ n;
  @      memcmp(s1,s2,n) == 0 && strlen(s2) < n ==> strlen(s1) == strlen(s2);
  @
  @ axiom memcmp_strlen_shift_left{L}:
  @   \forall char *s1, *s2; \forall ℤ k, n;
  @      memcmp(s1,s2 + k,n) == 0 && 0 <= k && strlen(s1) < n ==>
  @        0 <= strlen(s2) <= k + strlen(s1);
  @
  @ axiom memcmp_strlen_shift_right{L}:
  @   \forall char *s1, *s2; \forall ℤ k, n;
  @      memcmp(s1 + k,s2,n) == 0 && 0 <= k && strlen(s2) < n ==>
  @        0 <= strlen(s1) <= k + strlen(s2);
  @ }
  @*/

/*@ axiomatic StrCmp {
  @ logic ℤ strcmp{L}(char *s1, char *s2);
  @ // reads s1[0..strlen(s1)], s2[0..strlen(s2)];
  @
  @ axiom strcmp_zero{L}:
  @   \forall char *s1, *s2;
  @      strcmp(s1,s2) == 0 <==>
  @        (strlen(s1) == strlen(s2)
  @         && \forall ℤ i; 0 <= i <= strlen(s1) ==> s1[i] == s2[i]);
  @ }
  @*/

/*@ axiomatic StrNCmp {
  @ logic ℤ strncmp{L}(char *s1, char *s2, ℤ n);
  @ // reads s1[0..n-1], s2[0..n-1];
  @
  @ axiom strncmp_zero{L}:
  @   \forall char *s1, *s2; \forall ℤ n;
  @      strncmp(s1,s2,n) == 0 <==>
  @        (strlen(s1) < n && strcmp(s1,s2) == 0
  @         || \forall ℤ i; 0 <= i < n ==> s1[i] == s2[i]);
  @ }
  @*/

/*@ axiomatic StrChr {
  @ logic 𝔹 strchr{L}(char *s, ℤ c);
  @ // reads s[0..strlen(s)];
  @ // Returns [true] iff string [s] contains character [c]
  @
  @ axiom strchr_def{L}:
  @   \forall char *s; \forall ℤ c;
  @      strchr(s,c) <==> \exists ℤ i; 0 <= i <= strlen(s) && s[i] == c;
  @ }
  @*/

/*@ axiomatic WcsLen {
  @ logic ℤ wcslen{L}(wchar_t *s);
  @ // reads s[0..];
  @
  @ axiom wcslen_pos_or_null{L}:
  @   \forall wchar_t* s; \forall ℤ i;
  @      (0 <= i
  @       && (\forall ℤ j; 0 <= j < i ==> s[j] != L'\0')
  @       && s[i] == L'\0') ==> wcslen(s) == i;
  @
  @ axiom wcslen_neg{L}:
  @   \forall wchar_t* s;
  @      (\forall ℤ i; 0 <= i ==> s[i] != L'\0')
  @      ==> wcslen(s) < 0;
  @
  @ axiom wcslen_before_null{L}:
  @   \forall wchar_t* s; \forall int i; 0 <= i < wcslen(s) ==> s[i] != L'\0';
  @
  @ axiom wcslen_at_null{L}:
  @   \forall wchar_t* s; 0 <= wcslen(s) ==> s[wcslen(s)] == L'\0';
  @
  @ axiom wcslen_not_zero{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i <= wcslen(s) && s[i] != L'\0' ==> i < wcslen(s);
  @
  @ axiom wcslen_zero{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i <= wcslen(s) && s[i] == L'\0' ==> i == wcslen(s);
  @
  @ axiom wcslen_sup{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i && s[i] == L'\0' ==> 0 <= wcslen(s) <= i;
  @
  @ axiom wcslen_shift{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i <= wcslen(s) ==> wcslen(s+i) == wcslen(s)-i;
  @
  @ axiom wcslen_create{L}:
  @   \forall wchar_t* s; \forall int i;
  @      0 <= i && s[i] == L'\0' ==> 0 <= wcslen(s) <= i;
  @
  @ axiom wcslen_create_shift{L}:
  @   \forall wchar_t* s; \forall int i; \forall int k;
  @      0 <= k <= i && s[i] == L'\0' ==> 0 <= wcslen(s+k) <= i - k;
  @ }
  @*/

/*@ axiomatic WcsCmp {
  @ logic ℤ wcscmp{L}(wchar_t *s1, wchar_t *s2);
  @ // reads s1[0..wcslen(s1)], s2[0..wcslen(s2)];
  @
  @ axiom wcscmp_zero{L}:
  @   \forall wchar_t *s1, *s2;
  @      wcscmp(s1,s2) == 0 <==>
  @        (wcslen(s1) == wcslen(s2)
  @         && \forall ℤ i; 0 <= i <= wcslen(s1) ==> s1[i] == s2[i]);
  @ }
  @*/

/*@ axiomatic WcsNCmp {
  @ logic ℤ wcsncmp{L}(wchar_t *s1, wchar_t *s2, ℤ n);
  @ // reads s1[0..n-1], s2[0..n-1];
  @
  @ axiom wcsncmp_zero{L}:
  @   \forall wchar_t *s1, *s2; \forall ℤ n;
  @      wcsncmp(s1,s2,n) == 0 <==>
  @        (wcslen(s1) < n && wcscmp(s1,s2) == 0
  @         || \forall ℤ i; 0 <= i < n ==> s1[i] == s2[i]);
  @ }
  @*/

/*@ logic ℤ minimum(ℤ i, ℤ j) = i < j ? i : j;
  @ logic ℤ maximum(ℤ i, ℤ j) = i < j ? j : i;
  @*/

/*@ predicate valid_string{L}(char *s) =
  @   0 <= strlen(s) && \valid(s+(0..strlen(s)));
  @
  @ predicate valid_string_or_null{L}(char *s) =
  @   s == \null || valid_string(s);
  @
  @ predicate valid_wstring{L}(wchar_t *s) =
  @   0 <= wcslen(s) && \valid(s+(0..wcslen(s)));
  @
  @ predicate valid_wstring_or_null{L}(wchar_t *s) =
  @   s == \null || valid_wstring(s);
  @*/

#define FRAMA_C_PTR __declspec(valid)
#define FRAMA_C_ARRAY(n) __declspec(valid_range(0,n))
#define FRAMA_C_STRING __declspec(valid_string)
#define FRAMA_C_STRING_OR_NULL __declspec(valid_string_or_null)
#define FRAMA_C_WSTRING __declspec(valid_wstring)
#define FRAMA_C_WSTRING_OR_NULL __declspec(valid_wstring_or_null)

#endif
