#ifndef _INCLUDE_conf1
#define _INCLUDE_conf1

/*
spécification de l'opérateur CONF1

_E1 : BOOLEAN
_S1 : BOOLEAN

Calcul :
_S1 = TRUE si la durée de l'état TRUE sur _E1 est >= Time et tant que _E1 = TRUE
_S1 = FALSE si la durée de l'état TRUE sur _E1 est < Time ou si _E1 = FALSE

Initialisation :
_S1 = FALSE
*/

#define CONF1(NNN, _E1, _Time, _S1) {\
static INTEGER CONF1_Timeout;\
INTEGER Horl_BR;\
   Horl_BR = M_Horloge_BR;\
   if (_E1)\
      {\
         if ((CONF1_Timeout)==0)\
            {\
               (CONF1_Timeout)=(Horl_BR)+(_Time);\
            }\
         (_S1)=(BOOLEAN)((Horl_BR)>=(CONF1_Timeout));\
      }\
   else\
      {\
         (CONF1_Timeout)=0;\
         (_S1)=FALSE;\
      }\
}

#endif

