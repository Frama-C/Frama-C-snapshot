(* Programmatic tests of the interval semantics of floating-point values in
   Fval. Run by fval_test.i. *)

open Fval

(* If true, prints each operation performed for the tests. Otherwise, only
   prints wrong operations. *)
let print = false

let report bug format =
  if print || bug
  then Kernel.result ("%s" ^^ format) (if bug then "BUG " else "")
  else Format.ifprintf Format.std_formatter format

(* Comparison between floats that distinguish between -0. and 0.. *)
module Float = struct
  [@@@ warning "-3"]
  external compare : float -> float -> int = "float_compare_total" "noalloc"
  [@@@ warning "+3"]
  let eq f1 f2 = compare f1 f2 = 0
  let le f1 f2 = compare f1 f2 <= 0
end

let round fkind =
  if fkind = Float32
  then Floating_point.round_to_single_precision_float
  else fun f -> f

(* Inject a float as a singleton interval. *)
let inject f =
  if classify_float f = FP_nan then nan else inject_singleton (F.of_float f)

(* Creates the interval [f1..f2]. *)
let inject_range f1 f2 = join (inject f1) (inject f2)

(* A list of interesting floating-point values. The functions below test the
   abstract semantics of Fval for all possible intervals built from these
   values. *)
let interesting_double =
  [-.infinity; -1.2e307; -120.; -1.2e-323; -0.;
     infinity;  1.3e308;  130.;  1.3e-323; +0.; ]

let interesting_float =
  [-.infinity; -2.2e37; -120.; -1.5e-33; -0.;
     infinity;  2.3e37;  130.;  1.6e-33; +0.; ]

let product itvs =
  let l = Extlib.product (fun a b -> (a, b)) itvs itvs in
  List.filter (fun (a, b) -> Float.le a b) l

let interesting = function
  | Float32 | Real -> interesting_float
  | Float64 -> interesting_double

(* Tests the abstract operation [fval_op fval = fval_r] by adding
   NaN to fval. The function checks that the result is [fval_r] plus NaN. *)
let test_unop_for_nan ?(exact=true) fkind fval_op sop fval fval_r =
  let fval_nan = join nan fval in
  let fval_r_nan = join nan fval_r in
  let check f =
    let f_r = fval_op fkind f in
    let bug = not ((if exact then equal else is_included) fval_r_nan f_r) in
    report bug
      "NAN %s %a -> %a  &&  %a"
      sop pretty f pretty fval_r_nan pretty f_r
  in
  check fval_nan

(* Tests the abstract operation [fval_op] on interval [b..e].
   [op] is the concrete caml operation. *)
let test_unop_on_itv ?exact fkind op fval_op str_op (b, e) =
  let round = round fkind in
  let b, e = round b, round e in
  let fval = inject_range b e in
  let fval_r = fval_op fkind fval in
  test_unop_for_nan ?exact fkind fval_op str_op fval fval_r;
  if Float.eq b e
  then
    let r = round (op b) in
    let bug = not (is_included (inject r) fval_r)  in
    let bug = bug || (fkind <> Real && not (is_singleton fval_r)) in
    report bug
      "SINGLE %s %F -> %F || %a"
      str_op b r pretty fval_r;
  else
    let check x =
      if Float.le b x && Float.le x e
      then
        let r = round (op x) in
        let bug = not (is_included (inject r) fval_r) in
        report bug "ITV %s %F -> %F  &&  %s %a -> %a"
          str_op x r str_op pretty fval pretty fval_r
    in
    List.iter check (interesting fkind)

(* Tests the abstract operation [fval_1 fval_op fval_2 = fval_r] by adding
   NaN to fval_1 and fval_2. The result must then be [fval_r] plus NaN, except
   for the pow operator, as [pow 1 NaN] = [pow NaN 0] = 1 (and not NaN). The
   argument [pow] must be true if [fval_op] is pow. *)
let test_binop_for_nan ~pow fkind fval_op sop fval_1 fval_2 fval_r =
  let fval_1_nan = join nan fval_1 in
  let fval_2_nan = join nan fval_2 in
  let fval_r_nan = join nan fval_r in
  let check ~nan f_1 f_2 =
    let f_r = fval_op fkind f_1 f_2 in
    let expected_r = if nan then fval_r_nan else fval_r in
    let bug = not (equal f_r expected_r) in
    report bug
      "NAN %a %s %a -> %a  &&  %a"
      pretty f_1 sop pretty f_2 pretty expected_r pretty f_r
  in
  let nan_1 = not (pow && is_included fval_2 zeros) in
  check ~nan:nan_1 fval_1_nan fval_2;
  let nan_2 = not (pow && equal (inject 1.) fval_1) in
  check ~nan:nan_2 fval_1 fval_2_nan;
  check ~nan:true fval_1_nan fval_2_nan

(* Tests the abstract operation [fval_op] on intervals [b1..e1] and [b2..e2].
   [op] is the concrete caml operation. *)
let test_binop_on_itv ~pow fkind op fval_op str_op ((b1, e1), (b2, e2)) =
  let round = round fkind in
  let b1, e1, b2, e2 = round b1, round e1, round b2, round e2 in
  let fval_1 = inject_range b1 e1 in
  let fval_2 = inject_range b2 e2 in
  let fval_r = fval_op fkind fval_1 fval_2 in
  test_binop_for_nan ~pow fkind fval_op str_op fval_1 fval_2 fval_r;
  if Float.eq b1 e1 && Float.eq b2 e2
  then
    let r = round (op b1 b2) in
    let bug = not (is_included (inject r) fval_r)  in
    let bug = bug || (fkind <> Real && not (is_singleton fval_r)) in
    report bug
      "SINGLE %F %s %F -> %F  &&  %a"
      b1 str_op b2 r pretty fval_r;
  else
    let check x y  =
      if Float.le b1 x && Float.le x e1 && Float.le b2 y && Float.le y e2
      then begin
        let r = round (op x y) in
        let bug = not (is_included (inject r) fval_r) in
        report bug "ITV %F %s %F -> %F  &&  %a %s %a -> %a"
          x str_op y r pretty fval_1 str_op pretty fval_2 pretty fval_r
      end
    in
    let interesting = interesting fkind in
    List.iter (fun x -> List.iter (fun y -> check x y) interesting) interesting

let pretty_truth fmt = let open Abstract_interp in function
    | True -> Format.pp_print_string fmt "True"
    | False -> Format.pp_print_string fmt "False"
    | Unknown -> Format.pp_print_string fmt "Unknown"

let test_comp_on_nan ~ne fval_op sop fval_1 fval_2 =
  let fval_1_nan = join nan fval_1 in
  let fval_2_nan = join nan fval_2 in
  let check f_1 f_2 =
    let f_r = forward_comp fval_op f_1 f_2 in
    let bug = f_r = Abstract_interp.Comp.(if ne then False else True) in
    report bug
      "NAN %a %s %a -> %b  &&  %a"
      pretty f_1 sop pretty f_2 ne pretty_truth f_r
  in
  check nan fval_2; check fval_1 nan;
  check fval_1_nan fval_2; check fval_1 fval_2_nan;
  check fval_1_nan fval_2_nan

let test_comp_on_itv ~ne op fval_op str_op ((b1, e1), (b2, e2)) =
  let fval_1 = inject_range b1 e1 in
  let fval_2 = inject_range b2 e2 in
  let fval_r = forward_comp fval_op fval_1 fval_2 in
  test_comp_on_nan ~ne fval_op str_op fval_1 fval_2;
  let check x y  =
    let r = op x y in
    let bug = fval_r = Abstract_interp.Comp.(if r then False else True) in
    report bug "COMP %F %s %F -> %b  &&  %a %s %a -> %a"
      x str_op y r pretty fval_1 str_op pretty fval_2 pretty_truth fval_r
  in
  check b1 b2; check b1 e2; check e1 b2; check e1 e2

external c_powf: float -> float -> float = "c_powf"

(* Round-trip of reinterpretation. Is the identity in the concrete. *)
let reinterpret fkind f =
  let signed = false in
  let i_f = Ival.inject_float f in
  let i_f_int, fkind = match fkind with
    | Float32 ->
      Ival.reinterpret_as_int i_f ~signed ~size:(Integer.of_int 32),
      Cil_types.FFloat
    | Float64 ->
      Ival.reinterpret_as_int i_f ~signed ~size:(Integer.of_int 64),
      Cil_types.FDouble
    | Real -> assert false
  in
  let i_f' = Ival.reinterpret_as_float fkind i_f_int in
  Ival.project_float i_f'

let test_forward_unop () =
  let test_unop ?exact fkind op fval_op str_op =
    let l2 = product (interesting fkind) in
    let str_op = Format.asprintf "%s%a" str_op pretty_kind fkind in
    List.iter (test_unop_on_itv ?exact fkind op fval_op str_op) l2
  in
  let test_unop ?exact op ?(fkinds=[Float32; Float64; Real]) fval_op str_op =
    List.iter (fun fkind -> test_unop ?exact fkind op fval_op str_op) fkinds
  in
  test_unop ( ~-. ) (fun _ -> neg) "-.";
  test_unop Pervasives.sqrt sqrt "sqrt";
  test_unop Pervasives.log log "log";
  test_unop Pervasives.log10 log10 "log10";
  test_unop Pervasives.exp exp "exp";
  test_unop Pervasives.floor floor "floor";
  test_unop Pervasives.ceil ceil "ceil";
  (* TODO: use interesting floating-point values for trigonometry. *)
  test_unop ~fkinds:[Float64] Pervasives.cos cos "cos";
  test_unop ~fkinds:[Float32] Floating_point.cosf cos "cos";
  test_unop ~fkinds:[Float64] Pervasives.sin sin "sin";
  test_unop ~fkinds:[Float32] Floating_point.sinf sin "sin";
  test_unop ~exact:false ~fkinds:[Float32; Float64]
    (fun f -> f) reinterpret "reinterpret";
;;

let test_forward_binop () =
  let test_binop ~pow fkind op fval_op str_op =
    let l2 = product (interesting fkind) in
    let l4 = Extlib.product (fun a b -> (a, b)) l2 l2 in
    let str_op = Format.asprintf "%s%a" str_op pretty_kind fkind in
    List.iter (test_binop_on_itv ~pow fkind op fval_op str_op) l4
  in
  let test_binop ~pow fkinds op fval_op str_op =
    List.iter (fun fkind -> test_binop ~pow fkind op fval_op str_op) fkinds
  in
  let fkinds = [Float32; Float64; Real] in
  test_binop ~pow:false fkinds ( +. ) add "*";
  test_binop ~pow:false fkinds ( -. ) sub "*";
  test_binop ~pow:false fkinds ( *. ) mul "*";
  test_binop ~pow:false fkinds ( /. ) div "/";
  test_binop ~pow:false [Float32; Float64] mod_float fmod "mod";
  test_binop ~pow:true [Float64] ( ** ) pow "pow";
  test_binop ~pow:true [Float32] Floating_point.powf pow "pow";
  test_binop ~pow:false [Float64] Pervasives.atan2 atan2 "atan2";
  test_binop ~pow:false [Float32] Floating_point.atan2f atan2 "atan2"

let interesting_for_comp =
  [-.infinity; -1.2e-323; -0.; infinity; 1.3e-323; +0.; ]

let test_forward_comp () =
  let test_comp ~ne op fval_op str_op =
    let l2 = product interesting_for_comp in
    let l4 = Extlib.product (fun a b -> (a, b)) l2 l2 in
    List.iter (test_comp_on_itv ~ne op fval_op str_op) l4
  in
  let ne = false in
  let open Abstract_interp in
  test_comp ~ne (=) Comp.Eq "==";
  test_comp ~ne (<) Comp.Lt "<";
  test_comp ~ne (<=) Comp.Le "<=";
  test_comp ~ne (>) Comp.Gt ">";
  test_comp ~ne (>=) Comp.Ge ">=";
  test_comp ~ne:true (<>) Comp.Ne "<>"

let main _ =
  test_forward_unop ();
  test_forward_binop ();
  test_forward_comp ();
;;

let () = Db.Main.extend main
