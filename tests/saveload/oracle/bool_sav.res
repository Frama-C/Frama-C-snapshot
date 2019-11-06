[kernel] Parsing tests/saveload/bool.c (with preprocessing)
[eva] Analyzing a complete application starting at main
[eva] Computing initial state
[eva] Initial state computed
[eva:initial-state] Values of globals at initialization
  x ∈ {0}
  y ∈ {0}
[eva] computing for function f <- main.
  Called from tests/saveload/bool.c:25.
[eva] tests/saveload/bool.c:17: assertion got status valid.
[eva] tests/saveload/bool.c:18: starting to merge loop iterations
[eva:alarm] tests/saveload/bool.c:18: Warning: 
  signed overflow. assert -2147483648 ≤ i - 1;
[eva] Recording results for f
[eva] Done for function f
[eva] computing for function printf_va_1 <- main.
  Called from tests/saveload/bool.c:27.
[eva] using specification for function printf_va_1
[eva] tests/saveload/bool.c:27: 
  function printf_va_1: precondition got status valid.
[eva] Done for function printf_va_1
[eva] computing for function printf_va_2 <- main.
  Called from tests/saveload/bool.c:29.
[eva] using specification for function printf_va_2
[eva] tests/saveload/bool.c:29: 
  function printf_va_2: precondition got status valid.
[eva] Done for function printf_va_2
[eva] computing for function printf_va_3 <- main.
  Called from tests/saveload/bool.c:31.
[eva] using specification for function printf_va_3
[eva] tests/saveload/bool.c:31: 
  function printf_va_3: precondition got status valid.
[eva] Done for function printf_va_3
[eva] computing for function printf_va_4 <- main.
  Called from tests/saveload/bool.c:33.
[eva] using specification for function printf_va_4
[eva] tests/saveload/bool.c:33: 
  function printf_va_4: precondition got status valid.
[eva] Done for function printf_va_4
[eva] computing for function printf_va_5 <- main.
  Called from tests/saveload/bool.c:35.
[eva] using specification for function printf_va_5
[eva] tests/saveload/bool.c:35: 
  function printf_va_5: precondition got status valid.
[eva] Done for function printf_va_5
[eva] Recording results for main
[eva] done for function main
[eva] ====== VALUES COMPUTED ======
[eva:final-states] Values at end of function f:
  i ∈ [-2147483648..9]
  j ∈ {5}
  __retres ∈ {0}
[eva:final-states] Values at end of function main:
  x ∈ {1}
  y ∈ {2}
  S___fc_stdout[0..1] ∈ [--..--]
