[kernel] Parsing share/libc/__fc_builtin_for_normalization.i (no preprocessing)
[kernel] Parsing tests/saveload/basic.i (no preprocessing)
[value] Analyzing a complete application starting at main
[value] Computing initial state
[value] Initial state computed
[value] Values of globals at initialization
  
tests/saveload/basic.i:18:[value] Assertion got status valid.
tests/saveload/basic.i:19:[value] entering loop for the first time
tests/saveload/basic.i:19:[kernel] warning: signed overflow. assert -2147483648 ≤ i-1;
[value] Recording results for main
[value] done for function main
[value] ====== VALUES COMPUTED ======
[value] Values at end of function main:
  i ∈ [-2147483648..2147483646]
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
