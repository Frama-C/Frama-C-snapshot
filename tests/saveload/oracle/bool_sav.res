[kernel] Parsing tests/saveload/bool.c (with preprocessing)
[value] Analyzing a complete application starting at main
[value] Computing initial state
[value] Initial state computed
[value:initial-state] Values of globals at initialization
  __fc_errno ∈ [--..--]
  __fc_stdin ∈ {{ NULL ; &S___fc_stdin[0] }}
  __fc_stdout ∈ {{ NULL ; &S___fc_stdout[0] }}
  __fc_fopen[0..15] ∈ {0}
  __fc_p_fopen ∈ {{ &__fc_fopen[0] }}
  x ∈ {0}
  y ∈ {0}
  S___fc_stdin[0..1] ∈ [--..--]
  S___fc_stdout[0..1] ∈ [--..--]
[value] computing for function f <- main.
    Called from tests/saveload/bool.c:25.
tests/saveload/bool.c:17:[value] assertion got status valid.
tests/saveload/bool.c:18:[value] entering loop for the first time
tests/saveload/bool.c:18:[value] warning: signed overflow. assert -2147483648 ≤ i - 1;
[value] Recording results for f
[value] Done for function f
[value] computing for function printf_va_1 <- main.
    Called from tests/saveload/bool.c:27.
[value] using specification for function printf_va_1
tests/saveload/bool.c:27:[value] function printf_va_1: precondition got status valid.
[value] Done for function printf_va_1
[value] computing for function printf_va_2 <- main.
    Called from tests/saveload/bool.c:29.
[value] using specification for function printf_va_2
tests/saveload/bool.c:29:[value] function printf_va_2: precondition got status valid.
[value] Done for function printf_va_2
[value] computing for function printf_va_3 <- main.
    Called from tests/saveload/bool.c:31.
[value] using specification for function printf_va_3
tests/saveload/bool.c:31:[value] function printf_va_3: precondition got status valid.
[value] Done for function printf_va_3
[value] computing for function printf_va_4 <- main.
    Called from tests/saveload/bool.c:33.
[value] using specification for function printf_va_4
tests/saveload/bool.c:33:[value] function printf_va_4: precondition got status valid.
[value] Done for function printf_va_4
[value] computing for function printf_va_5 <- main.
    Called from tests/saveload/bool.c:35.
[value] using specification for function printf_va_5
tests/saveload/bool.c:35:[value] function printf_va_5: precondition got status valid.
[value] Done for function printf_va_5
[value] Recording results for main
[value] done for function main
[value] ====== VALUES COMPUTED ======
[value:final-states] Values at end of function f:
  i ∈ [-2147483648..9]
  j ∈ {5}
  __retres ∈ {0}
[value:final-states] Values at end of function main:
  x ∈ {1}
  y ∈ {2}
  S___fc_stdout[0..1] ∈ [--..--]
