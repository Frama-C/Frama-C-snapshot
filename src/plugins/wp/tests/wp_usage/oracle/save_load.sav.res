[kernel] Parsing tests/wp_usage/save_load.i (no preprocessing)
[wp] Running WP plugin...
[wp] Loading driver 'share/wp.driver'
[wp] Warning: Missing RTE guards
------------------------------------------------------------
  Function f
------------------------------------------------------------

Goal Post-condition (file tests/wp_usage/save_load.i, line 16) in 'f':
Assume {
  Type: is_sint32(a) /\ is_sint32(b) /\ is_sint32(b_1) /\ is_sint32(c).
  If c != 0
  Then { Have: b_1 = b. }
  Else { Have: (1 + b_1) = b. }
}
Prove: 0 < (a + b).

------------------------------------------------------------
