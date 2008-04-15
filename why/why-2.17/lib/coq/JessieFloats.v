
(* Why model for floating-point numbers
   Implements the file lib/why/floats_full.why *)

Require Export WhyFloats.



Inductive float_format : Set :=  Single | Double.

Definition bfloat_format (f : float_format) :=
match f with 
| Single => bsingle
| Double => bdouble
end.

Definition precision (f : float_format) :=
match f with 
| Single => 24%nat
| Double => 53%nat
end.

Definition max_gen_float (f : float_format) :=
match f with 
| Single => ((2-powerRZ radix (-23))*powerRZ radix 127)%R
| Double => ((2-powerRZ radix (-52))*powerRZ radix 1023)%R
end.

(*
Definition plus_infinity_gen_float (f : float_format) :=
match f with 
| Single => (powerRZ radix 128)%R
| Double => (powerRZ radix 1024)%R
end.
*)


(* generic floats*)

Record gen_float : Set := mk_gen_float {
   genf         : float;
   exact_value : R;
   model_value : R
 }.




Definition float_value (x:gen_float) := FtoRradix (genf x).

Definition gen_round_error (x:gen_float) := 
    (Rabs ((exact_value x) - (float_value x))).

Definition gen_total_error (x:gen_float):= 
    (Rabs ((model_value x) - (float_value x))).


Definition gen_set_model (x:gen_float) (r:R) :=
      mk_gen_float (genf x) (exact_value x) r.


Definition  gen_float_of_real_aux (f:float_format) (m:mode) (r r1 r2:R) := 
match m with
  |  nearest_even => mk_gen_float (RND_EvenClosest (bfloat_format f) radix (precision f) r) 
                                 r1 r2
  |  to_zero          => mk_gen_float (RND_Zero (bfloat_format f) radix (precision f) r) 
                                 r1 r2
  |  up                  => mk_gen_float (RND_Max (bfloat_format f) radix (precision f) r) 
                                 r1 r2
  |  down             => mk_gen_float (RND_Min (bfloat_format f) radix (precision f) r) 
				r1 r2
  |  nearest_away => mk_gen_float (RND_ClosestUp (bfloat_format f) radix (precision f) r) 
				r1 r2
  end.

   
Definition gen_float_of_real (f:float_format) (m:mode) (r:R) := gen_float_of_real_aux f m r r r.
   

Definition round_float (f:float_format) (m:mode) (r:R) := FtoRradix (genf (gen_float_of_real f m r)).








