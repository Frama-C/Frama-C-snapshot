[kernel] Parsing share/libc/__fc_builtin_for_normalization.i (no preprocessing)
[kernel] Parsing tests/saveload/callbacks.i (no preprocessing)
[value] Analyzing a complete application starting at main1
[value] Computing initial state
[value] Initial state computed
[value] Values of globals at initialization
  x ∈ {0}
  y ∈ {0}
[value] computing for function g1 <- main1.
        Called from tests/saveload/callbacks.i:25.
[value] computing for function f <- g1 <- main1.
        Called from tests/saveload/callbacks.i:16.
[value] Recording results for f
[from] Computing for function f
[from] Done for function f
[value] Done for function f
[value] Recording results for g1
[from] Computing for function g1
[from] Done for function g1
[value] Done for function g1
[value] computing for function g2 <- main1.
        Called from tests/saveload/callbacks.i:26.
[value] computing for function f <- g2 <- main1.
        Called from tests/saveload/callbacks.i:21.
[value] Recording results for f
[from] Computing for function f
[from] Done for function f
[value] Done for function f
[value] Recording results for g2
[from] Computing for function g2
[from] Done for function g2
[value] Done for function g2
[value] Recording results for main1
[from] Computing for function main1
[from] Done for function main1
[value] done for function main1
[from] ====== DISPLAYING CALLWISE DEPENDENCIES ======
[from] call to f at tests/saveload/callbacks.i:16 (by g1):
  x FROM p
[from] call to f at tests/saveload/callbacks.i:21 (by g2):
  y FROM p
[from] call to g1 at tests/saveload/callbacks.i:25 (by main1):
  x FROM \nothing
[from] call to g2 at tests/saveload/callbacks.i:26 (by main1):
  y FROM \nothing
[from] entry point:
  x FROM \nothing
  y FROM \nothing
[from] ====== END OF CALLWISE DEPENDENCIES ======
[inout] Out (internal) for function f:
          x; y
[inout] Out (internal) for function g1:
          x
[inout] Out (internal) for function g2:
          y
[inout] Out (internal) for function main1:
          x; y
