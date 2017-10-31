/* This code is AIRBUS property */

#ifndef _INCLUDE_prec_i
#define _INCLUDE_prec_i

/*
 spécification de l'opérateur PREC_I

_E1 : INTEGER
_S1 : INTEGER

Calcul :
_S1[k] = _E1[k-1]

Initialisation :
_E1[k-1] = FALSE
*/

#define PREC_I(NNN, _E1, _S1) {\
static INTEGER PREC_I_RE1;\
   (_S1)=PREC_I_RE1;\
   PREC_I_RE1=(_E1);\
}

#endif

