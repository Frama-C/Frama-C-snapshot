[kernel] Parsing tests/saveload/segfault_datatypes.i (no preprocessing)
[value] Analyzing a complete application starting at main
[value] Computing initial state
[value] Initial state computed
[value:initial-state] Values of globals at initialization
  
[value] tests/saveload/segfault_datatypes.i:13: entering loop for the first time
[value:alarm] tests/saveload/segfault_datatypes.i:13: Warning: 
  signed overflow. assert -2147483648 ≤ i - 1;
[value] Recording results for main
[value] done for function main
[value] ====== VALUES COMPUTED ======
[value:final-states] Values at end of function main:
  i ∈ [-2147483648..9]
  j ∈ {5}
  __retres ∈ {0}
[from] Computing for function main
[from] Done for function main
[from] ====== DEPENDENCIES COMPUTED ======
  These dependencies hold at termination for the executions that terminate:
[from] Function main:
  \result FROM \nothing
[from] ====== END OF DEPENDENCIES ======
[inout] Out (internal) for function main:
    i; j; tmp; __retres
[inout] Inputs for function main:
    \nothing
