[kernel] Parsing tests/wp_usage/save_load.i (no preprocessing)
[wp] Running WP plugin...
[wp] Loading driver 'share/wp.driver'
[wp] warning: Missing RTE guards
------------------------------------------------------------
  Function f
------------------------------------------------------------

Goal Post-condition (file tests/wp_usage/save_load.i, line 16) in 'f':
Assume {
  Type: is_sint32(a) /\ is_sint32(b) /\ is_sint32(b_1) /\ is_sint32(c) /\
      is_sint32(f) /\ is_sint32(f - b_1).
  If c != 0
  Then { Have: (b_1 = b) /\ ((1 + a + b_1) = f). }
  Else { Have: ((1 + b_1) = b) /\ ((a + b) = (1 + f)). }
}
Prove: 0 < (a + b).

------------------------------------------------------------
