
Require Why.

(*Why*) Parameter f1 :
  (y: Z)(r: Z)(sig_2 Z unit [r0: Z][result: unit]((q1 r0 r y))).


(*Why*) Parameter f :
  (x: Z)(t: (array Z))
  (sig_2 (array Z) unit [t0: (array Z)][result: unit]((q t0 t x))).


