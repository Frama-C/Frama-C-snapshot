(* minimal set of defs for using gappa *)

Require Export Gappa_tactic.
Require Export Reals.

Inductive float_format : Set :=  Single | Double.

Definition max_gen_float (f : float_format) :=
match f with 
| Single => (* ((2-powerRZ 2 (-23))*powerRZ 2 127)%R *)
(16777215*powerRZ 2 104)%R
| Double => 
  (* (2 - 2 ^ (-52)) * 2 ^ 1023 = 2 ^ 1024 - 2 ^ 971 = (2^53 - 1) * 2^ 971 *)
  (9007199254740991 * powerRZ 2 971)%R
end.

Definition mode := round_dir.

Definition nearest_even := roundNE.

Definition round_float (f : float_format) (mode : mode) x :=
  match f with
| Single => gappa_rounding (rounding_float mode 24 149) x
| Double => gappa_rounding (rounding_float mode 53 1074) x
end.

Parameter gen_float : Set.
Parameter float_value : gen_float -> R.
Parameter exact_value : gen_float -> R.
Parameter model_value : gen_float -> R.