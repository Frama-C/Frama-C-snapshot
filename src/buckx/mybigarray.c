/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: mybigarray.c,v 1.1 2008/04/01 09:25:20 uid568 Exp $ */

#include "caml/bigarray.h"
#include "caml/mlvalues.h"


value mybigarray_alignment (value ba)
{
  long data;
#ifndef Caml_ba_data_val
  data = (long)Data_bigarray_val(ba);
#else
  data = (long)Caml_ba_data_val(ba);
#endif
  return Val_long(data);
}

