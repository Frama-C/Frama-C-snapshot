[eva] Analyzing a complete application starting at main
[eva] Computing initial state
[eva] Initial state computed
[eva:initial-state] Values of globals at initialization
  x ∈ {0x1.0000000000000p0}
  y ∈ {0}
  z ∈ {0}
  t ∈ {0}
  min_f ∈ {0}
  min_fl ∈ {0}
  den ∈ {0}
[eva] computing for function Frama_C_interval <- main.
  Called from tests/float/absorb.c:15.
[eva] using specification for function Frama_C_interval
[eva] tests/float/absorb.c:15: 
  function Frama_C_interval: precondition 'order' got status valid.
[eva] Done for function Frama_C_interval
[eva] tests/float/absorb.c:18: starting to merge loop iterations
[eva] Recording results for main
[eva] done for function main
[eva] ====== VALUES COMPUTED ======
[eva:final-states] Values at end of function main:
  Frama_C_entropy_source ∈ [--..--]
  x ∈ {0x1.0000000000000p0}
  y ∈ {0x1.0000000000000p0}
  z ∈ {0}
  t ∈ [-0x1.bc16d60000000p61 .. 0x1.bc16d60000000p61]
  min_f ∈ {0x1.0000000000000p-126}
  min_fl ∈ {-0x1.0000000000000p-126}
  den ∈ {0x1.0000000000000p-133}
  b ∈ [-4000000004000000001..4000000004000000001]
