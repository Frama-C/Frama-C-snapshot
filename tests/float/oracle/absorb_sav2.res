[value] Analyzing a complete application starting at main
[value] Computing initial state
[value] Initial state computed
[value:initial-state] Values of globals at initialization
  Frama_C_entropy_source ∈ [--..--]
  x ∈ {0x1.0000000000000p0}
  y ∈ {0}
  z ∈ {0}
  t ∈ {0}
  min_f ∈ {0}
  min_fl ∈ {0}
  den ∈ {0}
[value] computing for function Frama_C_interval <- main.
  Called from tests/float/absorb.c:15.
[value] using specification for function Frama_C_interval
[value] tests/float/absorb.c:15: 
  function Frama_C_interval: precondition 'order' got status valid.
[value] Done for function Frama_C_interval
[value] tests/float/absorb.c:18: entering loop for the first time
[value] Recording results for main
[value] done for function main
[value] ====== VALUES COMPUTED ======
[value:final-states] Values at end of function main:
  Frama_C_entropy_source ∈ [--..--]
  x ∈ {0x1.0000000000000p0}
  y ∈ {0x1.0000000000000p0}
  z ∈ {0}
  t ∈ [-0x1.bc16d60000000p61 .. 0x1.bc16d60000000p61]
  min_f ∈ {0x1.0000000000000p-126}
  min_fl ∈ {-0x1.0000000000000p-126}
  den ∈ {0x1.0000000000000p-133}
  b ∈ [-4000000004000000001..4000000004000000001]
