(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eval

type function_calls =
  | FullInterprocedural
  (* The state in the caller is inlined in the callee. Cannot remain precise
     in presence of calls to functions without a body. Makes Memexec really
     slow. *)
  | IntraproceduralAll
  (* Intraprocedural analysis only, we start a new function with an empty
     state ; this state is dropped at the end of a function, and we resume
     with the original state. Good for memexec. Unsound (see comments). *)
  | IntraproceduralNonReferenced
  (* Same as IntraproceduralAll, but only on non-global variables that
     are not referenced. Those variables cannot be modified by a callee,
     so this analysis is sound. Good for memexec. *)

(* Silence warning *)
let () = ignore
    [FullInterprocedural; IntraproceduralAll; IntraproceduralNonReferenced]

let function_calls_handling = ref IntraproceduralNonReferenced

module G = struct

  let opt2 f o1 o2 = match o1, o2 with
    | Some o1, Some o2 -> Some (f o1 o2)
    | _ -> None

  let cache_name s = Hptmap_sig.PersistentCache ("Value.Gauges." ^ s)

  module Bounds = struct
    include Datatype.Pair
        (Datatype.Option(Datatype.Integer)) (* lower bound, or -infty *)
        (Datatype.Option(Datatype.Integer)) (* upper bound, or +infty *)
    let pretty fmt (min, max: t) =
      match min, max with
      | Some min, Some max when Integer.equal min max ->
        Format.fprintf fmt "{%a}" Abstract_interp.Int.pretty min
      | _ ->
        let pp_bound sign fmt = function
          | None -> Format.fprintf fmt "%coo" sign
          | Some i -> Abstract_interp.Int.pretty fmt i
        in
        Format.fprintf fmt "[%a .. %a]" (pp_bound '-') min (pp_bound '+') max
    let pretty_debug = pretty

    let inject_range n1 n2 : t =
      Some (Integer.of_int (min n1 n2)), Some (Integer.of_int (max n1 n2))

    let enlarge i (b1, b2: t) : t =
      (Extlib.opt_map (Integer.min i) b1, Extlib.opt_map (Integer.max i) b2)

    let lift fmin fmax (bmin1, bmax1: t) (bmin2, bmax2: t) : t =
      (opt2 fmin bmin1 bmin2, opt2 fmax bmax1 bmax2)

    let equal (bmin1, bmax1: t) (bmin2, bmax2: t) =
      Extlib.opt_equal Integer.equal bmin1 bmin2 &&
      Extlib.opt_equal Integer.equal bmax1 bmax2

    let is_included (bmin1, bmax1: t) (bmin2, bmax2: t) =
      (match bmin1, bmin2 with
       | _, None -> true
       | None, Some _ -> false
       | Some b1, Some b2 -> Integer.le b2 b1)
      &&
      (match bmax1, bmax2 with
       | _, None -> true
       | None, Some _ -> false
       | Some b1, Some b2 -> Integer.le b1 b2)
    
    (* This function computes how much the bounds of [i2] have increased from
       those of [i1], i.e. [diff [1 .. 4]  [-2 .. 8]] is [-3 .. 4]
       and [diff [-2 .. 8] [1 .. 4]] is [-4 .. 3]. *)
    let delta (i1: t) (i2: t) : t =
      let min1, max1 = i1 in
      let min2, max2 = i2 in
      let delta_min = opt2 Integer.sub min2 min1 in
      let delta_max = opt2 Integer.sub max2 max1 in
      (* we may need to reorder the pointwise subtractions. See the second
         example above. *)
      let min = opt2 Integer.min delta_min delta_max in
      let max = opt2 Integer.max delta_min delta_max in
      min, max

    let join = lift Integer.min Integer.max
    let add = lift Integer.add Integer.add

    let narrow (min1, max1: t) (min2, max2: t) : t Bottom.or_bottom =
      let minb = match min1, min2 with
        | Some i1, Some i2 -> Some (Integer.max i1 i2)
        | None, i | i, None -> i
      in
      let maxb = match max1, max2 with
        | Some i1, Some i2 -> Some (Integer.min i1 i2)
        | None, i | i, None -> i
      in
      match minb, maxb with
      | Some min, Some max when Integer.lt max min -> `Bottom
      | min, max -> `Value (min, max)

    let succ (b1, b2: t): t =
      (Extlib.opt_map Integer.succ b1, Extlib.opt_map Integer.succ b2)

    let neg (bmin, bmax: t) : t =
      Extlib.opt_map Integer.neg bmax, Extlib.opt_map Integer.neg bmin

    let mul_ct k (bmin, bmax: t) : t =
      let mul = Integer.mul k in
      if Integer.le k Integer.zero then
        Extlib.opt_map mul bmax, Extlib.opt_map mul bmin
      else
        Extlib.opt_map mul bmin, Extlib.opt_map mul bmax

    let mul (bmin1, bmax1: t) (bmin2, bmax2 as b2: t) : t =
      (* multiplication by infty *)
      let mul_inf = function
        | None -> None
        | Some i as v ->
          if Integer.equal i Integer.zero then v else None
      in
      (* b2 * bmin1 *)
      let mulmin = match bmin1 with
        | None -> mul_inf bmax2, mul_inf bmin2
        | Some bmin1 -> mul_ct bmin1 b2
      in
      (* b2 * bmax1 *)
      let mulmax = match bmax1 with
        | None -> mul_inf bmin2, mul_inf bmax2
        | Some bmax1 -> mul_ct bmax1 b2
      in
      join mulmin mulmax

    let zero = Some Integer.zero, Some Integer.zero

    (* Widening between two bounds. Unstable bounds are widened to infty
       aggressively. This widening does not assumes that [is_included i1 i2]
       holds, unlike the widening of Ival. *)
    let widen ?threshold (min1, max1: t) (min2, max2: t) : t =
      let widen_unstable_min b1 b2 =
        if Extlib.opt_equal Integer.equal b1 b2 then b1 else None
      in
      let widen_unstable_max b1 b2 =
        match threshold with
        | None -> widen_unstable_min b1 b2
        | Some n ->
          (* more involved version that stops at n.  *)
          match b1, b2 with
          | None, _ | _, None -> None
          | Some ib1, Some ib2 ->
            if Integer.equal ib1 ib2 then b1
            else if Integer.le ib1 n && Integer.le ib2 n then Some n
            else None
      in
      (widen_unstable_min min1 min2, widen_unstable_max max1 max2)

    let to_ival (b1, b2: t) = Ival.inject_range b1 b2
    let from_ival (i: Ival.t) : t = Ival.min_and_max i

    type classify =
      | ContainsZero
      | Positive (* strictly *) of Integer.t * Integer.t option
      | Negative (* strictly *) of Integer.t option * Integer.t

    let classify_sign (min, max: t) =
      match min, max with
      | None, None -> ContainsZero
      | Some min, Some max ->
        if Integer.gt min Integer.zero
        then Positive (min, Some max)
        else if Integer.lt max Integer.zero
        then Negative (Some min, max)
        else ContainsZero
      | Some min, max ->
        if Integer.gt min Integer.zero
        then Positive (min, max)
        else ContainsZero
      | min, Some max ->
        if Integer.lt max Integer.zero
        then Negative (min, max)
        else ContainsZero

    let div_towards_minus_infty x y =
      if Integer.gt y Integer.zero
      then Integer.pos_div x y
      else Integer.(pos_div (neg x) (neg y))
    let div_towards_plus_infty x y =
      if Integer.lt y Integer.zero
      then Integer.pos_div x y
      else Integer.(pos_div (neg x) (neg y))

    (* Computes the possible [n] such that [(add b)^n = r], when [f^n]
       is [f] consecutive applications of [f]. *)
    let backward_nb ~(b:t) ~(r:Ival.t)  =
      let r = from_ival r in
      let nb_max =
        match classify_sign b with
        | ContainsZero ->
          None (* it is always possible to do any number of steps that
                  do not bring us closer to the goal *)
        | Positive (minb, _maxb) -> begin
            (* b is strictly positive, each iteration strictly increases *)
            match snd r with
            | None -> (* r is unbounded, an infinite number of iterations
                         is possible *)
              None
            | Some maxr ->
              if Integer.gt maxr Integer.zero then
                Some (div_towards_minus_infty maxr minb)
              else
                Some Integer.zero (* each iteration pulls us away *)
          end
        | Negative (_minb, maxb) -> begin
            (* Symmetric case *)
            match fst r with
            | None -> None
            | Some minr ->
              if Integer.lt minr Integer.zero then
                Some (div_towards_minus_infty minr maxb)
              else
                Some Integer.zero
          end
      in
      let nb_min =
        match classify_sign r with
        | ContainsZero ->
          Some Integer.zero (* already reached with 0 iterations *)
        | Positive (minr, _maxr) -> begin
            match snd b with
            | None -> (* max increment is variable, we cannot derive a bound *)
              Some Integer.zero
            | Some maxb ->
              if Integer.gt maxb Integer.zero then
                Some (div_towards_plus_infty minr maxb)
              else
                None (* bottom. Not currently returned *)
          end
        | Negative (_minr, maxr) -> begin (* symmetric *)
            match fst b with
            | None -> Some Integer.zero
            | Some minb ->
              if Integer.lt minb Integer.zero then
                Some (div_towards_plus_infty maxr minb)
              else
                None
          end
      in
      `Value (nb_min, nb_max)
    
  end

  (* A MV contains (usual) values for the different bases that are incremented
     in a loop.
     1. for missing bases, no information is stored (i.e. Top)
     2. bases are mapped to an interger range, or to a pointer
       2.1. bases can only be mapped to a pointer with a single base address
  *)
  module MV = struct

    include Hptmap.Make(Base)(Cvalue.V)(Hptmap.Comp_unused)
      (struct let v = [] end)
      (struct let l = [Ast.self] end)

    (* This function computes a pointwise union on two MVs assumed to have
       disjoint set of keys. *)
    let merge_disjoint =
      let cache = cache_name "MV.merge_disjoint" in
      let decide _ _ _ = assert false in
      join ~cache ~symmetric:true ~idempotent:false ~decide

    let empty_wh = Integer.zero, (fun _ -> Ival.Widen_Hints.empty)

    let widen =
      let cache = cache_name "MV.widen" in
      let decide _ b1 b2 = Some (Cvalue.V.widen empty_wh b1 b2) in
      inter ~cache ~symmetric:false ~idempotent:true ~decide

    let is_included =
      let cache = cache_name "MV.is_included" in
      let decide_fst _b _v1 = true (* v2 is top *) in
      let decide_snd _b _v2 = false (* v1 is top, v2 is not *) in
      let decide_both _ v1 v2 = Cvalue.V.is_included v1 v2 in
      let decide_fast s t =
        if s == t  || is_empty t (*all bases present in s but not in t
                                   are implicitly bound to Top in t,
                                   hence the inclusion holds *)
        then PTrue
        else PUnknown
    in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  end

  (* A MC contains, for interesting variables, the coefficient that is
     associated to one lambda, represented as an integer interval.
     Missing coefficients are 0. This is useful for variables that
     are not incremented in one inner, but only in outemost one. *)
  module MC = struct

    include Hptmap.Make(Base)(Bounds)(Hptmap.Comp_unused)
      (struct let v = [] end)
      (struct let l = [Ast.self] end)

    (* This function computes a pointwise union on two MCs assumed to have
       disjoint set of keys. *)
    let merge_disjoint =
      let cache = cache_name "MC.merge_disjoint" in
      let decide _ _ _ = assert false in
      join ~cache ~symmetric:true ~idempotent:false ~decide


    (* For the "standard" join and widen, keys present in one map but not
       in the other are assumed to be 0. *)

    let default = function None -> Bounds.zero | Some b -> b          

    let widen =
      let cache = cache_name "MC.widen" in
      let decide _ b1 b2 = Bounds.widen (default b1) (default b2) in
      generic_join ~cache ~symmetric:false ~idempotent:true ~decide

    let join =
      let cache = cache_name "MC.join" in
      let decide _ b1 b2 = Bounds.join (default b1) (default b2) in
      generic_join ~cache ~symmetric:true ~idempotent:true ~decide

    let is_included =
      let cache = cache_name "MC.is_included" in
      let decide_fst _b v1 = Bounds.(equal zero v1) in
      let decide_snd _b v2 = Bounds.(is_included zero v2) in
      let decide_both _ v1 v2 = Bounds.is_included v1 v2 in
      let decide_fast s t = if s == t then PTrue else PUnknown
    in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both


  end

  (* This function computes how much the bounds of [v2] have increased from
     those of [v1]. On pointers, we return a result in bytes, and only if the
     two variables point to the same base (invariant 2.1) *)
  let delta_min_max_cvalue v1 v2 =
    try
      let b1, i1 = Cvalue.V.find_lonely_key v1 in
      let b2, i2 = Cvalue.V.find_lonely_key v2 in
      if Base.equal b1 b2
      then Some (Bounds.delta (Ival.min_and_max i1) (Ival.min_and_max i2))
      else None
    with Not_found -> assert false (* invariant 2.1 of MV must already hold *)

  (* This function takes two mv, and 'subtracts' them for the [inc]
     operation of gauges. More precisely, for each base present in both maps,
     we subtract pointwise the min and max or their possible values.
     This is used to compute the 'difference' during one loop iteration. *)
  let delta_mv =
    let cache = cache_name "delta_mv" in
    let empty = MC.empty in
    let empty_left _ = empty in 
    let empty_right _ = empty in
    let both b v1 v2 =
      match delta_min_max_cvalue v1 v2 with
      | None -> MC.empty (* drop the base from the result *)
      | Some i -> MC.singleton b i
    in
    let join = MC.merge_disjoint in
    let f =
      MV.fold2_join_heterogeneous
        ~cache ~empty_left ~empty_right ~both ~join ~empty
    in
    fun mv1 mv2 -> f mv1 (MV.shape mv2)

  (* compute pointwise [mv - mc] *)
  let mv_minus_mc =
    let cache = cache_name "mv_minus_mc" in
    let empty = MV.empty in
    let empty_left _ = empty in
    let empty_right v = v in
    let both b v i =
      let bv, iv =
        try Cvalue.V.find_lonely_key v
        with Not_found -> assert false (* invariant 2.1 of MV *)
      in
      let i'_min, i'_max = Bounds.delta i (Ival.min_and_max iv) in
      let i' = Ival.inject_range i'_min i'_max in
      let v' = Cvalue.V.inject bv i' in
      MV.singleton b v'
    in
    let join = MV.merge_disjoint in
    let f =
      MV.fold2_join_heterogeneous
        ~cache ~empty_left ~empty_right ~both ~join ~empty
    in
    fun mv mc -> f mv (MC.shape mc)

  (* Implementation of the 'forget' operation. [nb] loop iterations have
     elapsed, and during one iteration, variables are incremented by [coeffs].
     Add [nb * coeffs] to [mv]. *)
  let forget nb mv mc =
    let cache = Hptmap_sig.NoCache in
    let empty = MV.empty in
    (* mv empty means that the variable is not tracked *)
    let empty_left _ = empty in
    (* mc empty means that the coefficient for the variable is 0. Return the
       [mv] component unchanged. *)
    let empty_right left = left in
    let both b v i =
      let p = Cvalue.V.inject_ival (Bounds.to_ival (Bounds.mul nb i)) in
      let v' = Cvalue.V.add_untyped ~factor:Int_Base.one v p in
      MV.singleton b v'
    in
    let join = MV.merge_disjoint in
    MV.fold2_join_heterogeneous
      ~cache ~empty_left ~empty_right ~both ~join ~empty mv (MC.shape mc)

  type multiple_iterations = { nb: Bounds.t; coeffs: MC. t}

  module MultipleIterations = struct

    let compare i1 i2 =
      let c = Bounds.compare i1.nb i2.nb in
      if c = 0 then MC.compare i1.coeffs i2.coeffs
      else c

    let _equal i1 i2 =
      Bounds.equal i1.nb i2.nb && MC.equal i1.coeffs i2.coeffs

    let hash i = Bounds.hash i.nb + 17 * MC.hash i.coeffs

    let structural_descr =
      Structural_descr.t_record [| Bounds.packed_descr; MC.packed_descr|]

    let succ i = { i with nb = Bounds.succ i.nb }

    let join i1 i2 =
      { nb = Bounds.join i1.nb i2.nb; coeffs = MC.join i1.coeffs i2.coeffs }

    (* Widen [i1] and [i2]. The number of iterations is widened only if
       [widen_nb] holds. *)
    let widen _stmt ~widen_nb i1 i2 =
      let nb =
        if widen_nb then
          let threshold =
            None (* LoopAnalysis.Loop_analysis.get_bounds _stmt *)
          in
          let threshold = Extlib.opt_map Integer.of_int threshold in
          let (min, max as w) = Bounds.widen ?threshold i1.nb i2.nb in
          (* Limit min bound to 0 *)
          if min = None then (Some Integer.zero, max) else w
        else
          Bounds.join i1.nb i2.nb
      in
      { nb; coeffs = MC.widen i1.coeffs i2.coeffs }

    (* Keep only the variables of [mi.coeffs] already present in [mv]. *)
    let restrict mv mi =
      { mi with coeffs = MC.inter_with_shape (MV.shape mv) mi.coeffs }

  end

  type iteration_info =
    PreciseIteration of int | MultipleIterations of multiple_iterations

  module IterationInfo = struct
    include Datatype.Make(struct
        type t = iteration_info
        let name = "Value.Gauges.IterationInfo"
        let reprs = [PreciseIteration 0]
        include Datatype.Serializable_undefined

        let compare ii1 ii2 = match ii1, ii2 with
          | PreciseIteration i1, PreciseIteration i2 ->
            Pervasives.compare i1 i2
          | MultipleIterations i1, MultipleIterations i2 ->
            MultipleIterations.compare i1 i2
          | PreciseIteration _, MultipleIterations _ -> -1
          | MultipleIterations _, PreciseIteration _ -> 1

        let hash = function
          | PreciseIteration i -> i
          | MultipleIterations i -> MultipleIterations.hash i

        let equal = Datatype.from_compare

        let structural_descr = Structural_descr.t_sum [|
          [| Datatype.Int.packed_descr|];
          [| Structural_descr.pack MultipleIterations.structural_descr |]
          |]
      end)

    let is_included i1 i2 =
      match i1, i2 with
      | PreciseIteration _, MultipleIterations _
      | MultipleIterations _, PreciseIteration _ -> false
      | PreciseIteration i1, PreciseIteration i2 -> i1 = i2
      | MultipleIterations m1, MultipleIterations m2 ->
        Bounds.is_included m1.nb m2.nb &&
        MC.is_included m1.coeffs m2.coeffs

    let restrict mv = function
      | PreciseIteration _ as pi -> pi
      | MultipleIterations mi ->
        MultipleIterations (MultipleIterations.restrict mv mi)

  end

  (* type t = MV.t * (stmt * iteration_info) list *)
  
  include Datatype.Pair_with_collections
      (MV)
      (Datatype.List(Datatype.Pair(Cil_datatype.Stmt)(IterationInfo)))
      (struct let module_name = "Values.Gauges_domain.G" end)
  
  let empty = MV.empty, []
  let top (state: t) : t =
    let top_iteration_info = function
      | PreciseIteration _ as p -> p
      | MultipleIterations m -> MultipleIterations { m with coeffs = MC.empty }
    in
    MV.empty, List.map (fun (s, ii) -> (s, top_iteration_info ii)) (snd state)

  let pretty_iteration_info fmt = function
    | PreciseIteration i -> Format.fprintf fmt "%s(%d)" "λ" i
    | MultipleIterations i ->
      Format.fprintf fmt "@[<v>@<1>%s(%a)@ @[%a@]@]"
        "λ" Bounds.pretty i.nb MC.pretty i.coeffs

  let pretty_loop_step fmt (stmt, ii) =
    Format.fprintf fmt "s%d: %a" stmt.sid pretty_iteration_info ii
  
  let pretty_loop_info =
    Pretty_utils.pp_list ~pre:"@[<v>" ~suf:"@]" ~sep:"@ " pretty_loop_step

  let pretty fmt (ct, l: t) =
    Format.fprintf fmt "@[<v>@[V: [%a]@]@ @[%a@]@]"
      MV.pretty ct pretty_loop_info l
  
  (* Abstract operation when returning at the start of a loop. Increment the
     counter for this loop, or correct the constants if the loop coefficients
     have already been found. *)
  let inc (s:t) : t =
    match s with
    | _, [] -> assert false
    | ct, ((stmt, i) :: q) ->
      match i with
      | PreciseIteration i ->
        (* Just increase the number of iterations *)
        ct, (stmt, PreciseIteration (i+1)) :: q
      | MultipleIterations i ->
        (* Correct the constant part w.r.t the coefficients *)
        let ct' = mv_minus_mc ct i.coeffs in
        let i' = MultipleIterations.succ i in
        ct', (stmt, MultipleIterations i') :: q

  (* We have determined that [coeffs] is going to be used for [ct], and [l]
     iterations have occurred. Remove [coeffs] [l] times. *)
  let remove_coeffs coeffs l ct =
    let rec aux l ct =
      if l = 0 then ct else aux (l-1) (mv_minus_mc ct coeffs)
    in
    aux l ct

  (* Remove from the coefficient maps the variables for which we have
     no initial value, for canonicity purposes. This occurs for example when
     a pointer points to multiple variables through multiple iterations. *)
  let restrict mv l =
    List.map (fun (s, mi) -> s, IterationInfo.restrict mv mi) l

  (* [l] is the number iteration in [s1], while it is [l+1] in [s2].
     Compute a slope, then remove [l] and [l+1] iterations from [ct1] and
     [ct2] accordingly. *)
  let join_consecutive_lambda l ct1 ct2 =
    let coeffs = delta_mv ct1 ct2 in
    coeffs,
    remove_coeffs coeffs l ct1,
    remove_coeffs coeffs (l+1) ct2

  (* join pointwise values. keys not present in one of the maps are dropped. *)
  let join_same_lambda =
    let cache = cache_name "join_same_lambda" in
    let decide _ v1 v2 =
      (* Forbid multiple pointers in the result *)
      try
        let b1, _i1 = Cvalue.V.find_lonely_key v1 in
        let b2, _i2 = Cvalue.V.find_lonely_key v2 in
        if Base.equal b1 b2 then
          Some (Cvalue.V.join v1 v2)
        else None
      with Not_found -> assert false (* invariant 2.1 of MV must already hold *)
    in
    MV.inter ~cache ~symmetric:true ~idempotent:true ~decide

  exception MessyJoin

  (* This function equalizes recursively the number of iterations in all
     the loops of [s1/s2]. When different precise number of iterations
     are merged, coefficients are computed. Returns the initial values
     for [s1] and [s2], the (common) join for the number of iterations,
     and a boolean indicating that at least one precise number of iterations
     has been  generalized into multiple iterations. *)
  let rec join_iterations (s1: t) (s2: t) =
    match s1, s2 with
    | (_, []), (_, _ :: _) | (_, _ :: _) , (_, []) ->
      raise MessyJoin (* should always be in the same number of loops *)

    | (ct1, []), (ct2, []) -> (** out of any loop *)
      ct1, ct2, [], false

    | (ct1, (stmt1, nb1) :: q1), (ct2, (stmt2, nb2) :: q2) ->
      if not (stmt1 ==  stmt2) then raise MessyJoin;
      let ct1, ct2, q, joined_iter = join_iterations (ct1, q1) (ct2, q2) in
      match nb1, nb2 with
      | MultipleIterations i1, MultipleIterations i2 ->
        (** Coefficients have already been found. Just merge the number of
            iterations. *)
        let i = MultipleIterations (MultipleIterations.join i1 i2) in
        ct1, ct2, (stmt1, i) :: q, joined_iter

      | PreciseIteration n1, PreciseIteration n2 ->
        (** Two exact number of iterations. If equal, do nothing. If not,
            infer coefficients, or go to top. *)
        let nb = Bounds.inject_range n1 n2 in
        let (ct1, ct2), nb, joined_iter = match n1 - n2 with
          | 0 -> (** Same number of iterations *)
            (ct1, ct2), PreciseIteration n1, false
          | 1 -> (** One more iteration in s1 *)
            let coeffs, ct1, ct2 = join_consecutive_lambda n2 ct2 ct1 in
            (ct1, ct2), MultipleIterations { nb; coeffs }, true
          | -1 -> (** One more iteration in s2 *)
            let coeffs, ct1, ct2 = join_consecutive_lambda n1 ct1 ct2 in
            (ct1, ct2), MultipleIterations { nb; coeffs }, true
          | _ -> (** difference > 1. This case does not happen with the
                     current iteration engine, and requires a division function
                     in module Bounds. Go to top *)
            (MV.empty, MV.empty),
            MultipleIterations { nb; coeffs = MC.empty }, true
        in
        ct1, ct2, (stmt1, nb) :: q, joined_iter

      | PreciseIteration i1, MultipleIterations m2 ->
        (** Normalizes the initial values [ct1] according to the coefficients
            computed in [m2] *)
        let ct1 = remove_coeffs m2.coeffs i1 ct1 in
        let nb = Bounds.enlarge (Integer.of_int i1) m2.nb in
        let ii = MultipleIterations { m2 with nb } in
        ct1, ct2, (stmt1, ii) :: q, true

      | MultipleIterations m1, PreciseIteration i2 ->
        let ct2 = remove_coeffs m1.coeffs i2 ct2 in
        let nb = Bounds.enlarge (Integer.of_int i2) m1.nb in
        let ii = MultipleIterations { m1 with nb } in
        ct1, ct2, (stmt1, ii) :: q, true

  (* full join: join the number of iterations, then join the initial values *)
  let join (s1: t) (s2: t) : t =
    try
      let ct1, ct2, q, _ = join_iterations s1 s2 in
      let ct = join_same_lambda ct1 ct2 in
      let q = restrict ct q in
      let r = (ct, q) in
      (* Kernel.result ~current:true "JOIN@.%a@.@.%a@.R@.%a"
           pretty s1 pretty s2 pretty r; *)
      r
    with MessyJoin -> empty

  let is_included (ct1, l1: t) (ct2, l2: t) =
    MV.is_included ct1 ct2 &&
    List.for_all2 (fun (_, i1) (_, i2) -> IterationInfo.is_included i1 i2) l1 l2

  (* debug version *)
  let _is_included s1 s2 =
    let r = is_included s1 s2 in
    Kernel.result ~current:true "INCL %b@.%a@.@.%a" r pretty s1 pretty s2;
    r
  
  (* hypothesis from Value: s2 is supposed to happen 'after' s1. This widening
     function is full of heuristics to maintain some precision, i.e. do not
     widen everything to Top immediately. Basically:
     - when coefficients have not yet been found in [s1], do not widen
       values. (Because they have incompatible shape. We can have <(l=4) x=8>
       in s1 and <l=[0..4], x=0+2l>, and it would be almost meaningless
       to widen 8 and [0].
     - widen the number of iterations only for the current loop
     - widen coefficients only if values have not been widened.
  *)
  let widen _kf stmt (_ct1, l1 as s1: t) (_ct2, _l2 as s2: t): t =
    (* We first perform a join, which will generalize the coefficients *)
    let ct1, ct2, lj, joined_iter = join_iterations s1 s2 in
    let ctj = join_same_lambda ct1 ct2 in
    let stmt_is_in_state = List.exists (fun (s', _) -> stmt == s') lj in
    (* Now we widen the bounds unstable between s1 and the join. We do so
       only if the coefficients are compatible enough, meaning that no
       precise iterations were generalized. This is to regain some precision,
       but may theoretically endanger termination. *)
    let ct = if joined_iter then ctj else MV.widen ct1 ctj in
    let rec widen_l l1 lj =
      match l1, lj with
      | [], [] -> []
      | [], _ | _, [] -> assert false
      | (stmt', i1) :: q1, (stmt'', ij) :: qj ->
        assert (stmt' == stmt'');
        let i =
          match i1, ij with
          | MultipleIterations _, PreciseIteration _ -> assert false
          | PreciseIteration _, PreciseIteration _ -> ij
          | PreciseIteration _, MultipleIterations _ ->
            ij (* do nothing in this case, the real widening will occur in
                  later iterations *)
          | MultipleIterations i1, MultipleIterations ij ->
            (* Only widen the loop counter if we are widening this loop.
               We make an exception when [stmt] appears nowhere in the list of
               loops, meaning that we are widening somewhere in a non-natural
               loop. *)
            let widen_nb = stmt == stmt' || not stmt_is_in_state in
            MultipleIterations (MultipleIterations.widen stmt ~widen_nb i1 ij)
        in
        if IterationInfo.equal i ij then
          (stmt', i) :: widen_l q1 qj (* find something to widen deeper *)
        else
          (stmt', i) :: qj
    in
    (* Widen list if coefficients have not been widened only. This may help
       precision, and should not endanger convergence. *)
    let l = if MV.equal ctj ct2 then widen_l l1 lj else lj in
    let l = restrict ct l in
    ct, l

  let narrow x _y = `Value x

  let enter_loop stmt (ct, l: t) : t =
    ct, (stmt, PreciseIteration 0) :: l

  let leave_loop stmt (ct, l:t) : t =
    match l with
    | [] -> assert false
    | (stmt', ii) :: q ->
      assert (stmt == stmt');
      match ii with
      | PreciseIteration _ ->
        (* the domain has not inferred anything yet *)
        (ct, q)
      | MultipleIterations mi ->
        (* increment [ct] by [mi.nb] iterations *)
        let ct' = forget mi.nb ct mi.coeffs in
        ct', q

  (* This function returns [true] if [vi] _may_ be tracked. Variables for
     which we return [false] will never be part of a state. *)
  let tracked_variable vi =
    Cil.isIntegralOrPointerType vi.vtype &&
    (match !function_calls_handling with
     | FullInterprocedural -> true
     | IntraproceduralAll -> not vi.vglob
     | IntraproceduralNonReferenced -> not vi.vglob && not vi.vaddrof
    ) &&
    not (Cil.typeHasQualifier "volatile" vi.vtype)

  let kill_base b (ct, l as state: t): t =
    let aux = function
      | (_, PreciseIteration _ as i) -> i
      | (s, MultipleIterations m) ->
        (s, MultipleIterations { m with coeffs = MC.remove b m.coeffs })
    in
    (* Synchronize this function with in_memory_variable *)
    match b with
    | Base.Var (vi, _) when tracked_variable vi ->
      MV.remove b ct, List.map aux l
    | _ -> state
  
  exception Untranslatable

  module Gauge = struct

    type t = Cvalue.V.t * Bounds.t list

    let pretty fmt (v, l: t) =
      Format.fprintf fmt "@[(%a,@ %a)@]" Cvalue.V.pretty v
        (Pretty_utils.pp_list ~pre:"" ~suf:"" ~sep:",@ " Bounds.pretty) l

    let _ = pretty (* silence warning *)

    (* assumes that [f x 0] = x *)
    let rec map2 f l1 l2 =
      match l1, l2 with
      | [], l | l, [] -> l (* all other coefficients are implicitly 0 *)
      | b1 :: l1, b2 :: l2 -> f b1 b2 :: map2 f l1 l2

    let on_cvalue_ival f v =
      try
        let v = Cvalue.V.project_ival v in
        Cvalue.V.inject_ival (f v)
      with Cvalue.V.Not_based_on_null -> raise Untranslatable

    let neg (ct, l: t) : t =
      on_cvalue_ival Ival.neg_int ct, List.map Bounds.neg l

    let mul_ct k (ct, l: t) : t =
      on_cvalue_ival (Ival.mul (Ival.inject_singleton k)) ct,
      List.map (Bounds.mul_ct k) l

    (* Try to find a multiplication by a constant, or give up *)
    let mul (ct1, l1 as g1 : t) (ct2, l2 as g2: t) =
      try
        match l1 with
        | [] ->
          let k = Ival.project_int (Cvalue.V.project_ival ct1) in
          mul_ct k g2
        | _ ->
          match l2 with
          | [] ->
            let k = Ival.project_int (Cvalue.V.project_ival ct2) in
            mul_ct k g1
          | _ -> raise Untranslatable
      with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int ->
        raise Untranslatable

    (* Check that [v] is an integer, or a single pointer (invariant 2 of MV) *)
    let sanitize_v v =
      try
        let _b, i = Cvalue.V.find_lonely_key v in
        match i with
        | Ival.Float _ -> raise Untranslatable
        | _ -> ()
      with Not_found -> raise Untranslatable

    let add (ct1, l1: t) (ct2, l2: t) : t =
      let ct = Cvalue.V.add_untyped ~factor:Int_Base.one ct1 ct2 in
      let l = map2 Bounds.add l1 l2 in
      sanitize_v ct;
      ct, l

    let sub g1 g2 = add g1 (neg g2)

    let ct x : t =
      sanitize_v x;
      (x, [])

  end

  (* Extract from [ct, l] the gauge corresponding to the variable [b].
     The gauge contains one coefficient per loop in the state. *)
  let extract_gauge (ct, l: t) b : Gauge.t option =
    try
      let ctb = MV.find b ct in
      let rec aux = function
        | [] -> []
        | (_, i) :: q ->
          let coeff =
            match i with
            | PreciseIteration _ ->
              Bounds.zero (* information is stored in [ctb] *)
            | MultipleIterations m ->
              try MC.find b m.coeffs
              with Not_found -> Bounds.zero
          in
          coeff :: aux q
      in
      Some (ctb, aux l)
    with Not_found -> None

  (* Evaluation of a gauge in a given state. The state is only used to find the
     current values for the loop counters. *)
  let eval_gauge (_ct, l: t) (ctg, lg: Gauge.t) =
    let rec aux l lg =
      match l, lg with
      | [], _ :: _ -> assert false
      | _, [] -> Bounds.zero
      | (_, PreciseIteration _) :: q, coeff :: qg ->
        assert (Bounds.equal coeff Bounds.zero);
        aux q qg
      | (_, MultipleIterations m) :: q, coeff :: qg ->
        (* [shift_b] is the amount [b] is incremented by the current
           loop *)
        let shift_b = Bounds.mul coeff m.nb in
        Bounds.add shift_b (aux q qg)
    in
    let shift = Cvalue.V.inject_ival (Bounds.to_ival (aux l lg)) in
    Cvalue.V.add_untyped ctg ~factor:Int_Base.one shift

  let backward_loop (ct, l: t) b v : t option =
    (* This function gather the non-zero coefficients for [b], together
       with the number of iterations of the relevant loops. *)
    let rec gather = function
      | [] -> []
      | (_, PreciseIteration _) :: q ->
        gather q (* for this loop, the information is still stored in [ct] *)
      | (stmt, MultipleIterations m) :: q ->
        try
          let c = MC.find b m.coeffs in
          if Bounds.equal c Bounds.zero then raise Not_found;
          (stmt, c, m.nb) :: gather q
        with Not_found ->
          (* not bound is equivalent to a coefficient of 0. Hence this loop
             does not modify [b] *)
          gather q
    in
    let rec replace stmt nb = function
      | [] -> assert false
      | (_, PreciseIteration _ as h) :: q -> h :: replace stmt nb q
      | (stmt', MultipleIterations m as h) :: q ->
        if stmt == stmt' then
          (stmt, MultipleIterations { m with nb }) :: q
        else
          h :: replace stmt nb q
    in
    try
      let ctb = MV.find b ct in
      (* compatibility between the bases should be ensured by the Cvalue
         domain, that will only allow values compatible with [b] *)
      let d = Cvalue.V.sub_pointwise v ctb in
      match gather l with
      | [] -> None
      | _ :: _ :: _ -> None (* TODO: linearize and solve *)
      | [(stmt, c, nb)] ->
        match Bounds.backward_nb ~b:c ~r:d with
        | `Bottom -> None (* TODO: return bottom *)
        | `Value n_iter ->
          match Bounds.narrow n_iter nb with
          | `Bottom -> None (* TODO: return bottom *)
          | `Value n_iter ->
            if not (Bounds.equal nb n_iter) then
              let l' = replace stmt n_iter l in
              Some (ct, l')
            else None
    with Not_found -> None 

  (* Convert a location into a supported variable, i.e. scalar. The location
     must assign the entire variable. Also check that the type of the
     variable fits [typ] *)
  let loc_to_base loc typ =
    try
      let locb = loc.Locations.loc in
      (* Single pointer *)
      let b, o = Locations.Location_Bits.find_lonely_binding locb in
      match b with
      | Base.Var (vi, Base.Known (_, max)) -> (* "standard" varinfos only *)
        if tracked_variable vi &&
           Cil_datatype.Typ.equal typ vi.vtype &&
           Ival.is_zero o &&
           (match loc.Locations.size with
            | Int_Base.Value size -> Integer.equal size (Integer.succ max)
            | Int_Base.Top -> false)
        then b
        else raise Untranslatable
      | _ -> raise Untranslatable
    with Not_found -> raise Untranslatable

  let gauge_from_state b (ct, l: t) : Gauge.t =
    try
      let ct = MV.find b ct in
      let rec aux = function
        | [] -> []
        | (_, iteration) :: q ->
          try
            match iteration with
            | PreciseIteration _ -> Bounds.zero :: aux q
            | MultipleIterations m -> MC.find b m.coeffs :: aux q
          with Not_found -> []
      in
      ct, aux l
    with Not_found -> raise Untranslatable

  let translate_exp state to_loc to_v e =
    let ptr_size e =
      let typ_pointed = Cil.typeOf_pointed (Cil.typeOf e) in
      try Integer.of_int (Cil.bytesSizeOf typ_pointed)
      with Cil.SizeOfError _ -> raise Untranslatable
    in
    (* This function translates the expression as a precise gauge. For any
       expression that cannot be handled, [Untranslatable] is raised. *)
    let rec aux_gauge e =
      match e.enode with
      | Const _ | SizeOf _ | SizeOfE _  | SizeOfStr _ | AlignOf _ | AlignOfE _
      | AddrOf _ | StartOf _ ->
        raise Untranslatable (* constant: using linearization directly *)

      | CastE (typ_dst ,e) ->
        fits_in_type ~is_cast:true typ_dst (aux e)

      | Lval lv ->
        let b = loc_to_base (to_loc lv) (Cil.typeOfLval lv) in
        gauge_from_state b state

      | UnOp (Neg , e, _) ->
        fits_in_type (Cil.typeOf e) (Gauge.neg (aux e))

      | UnOp ((BNot | LNot) ,_,_) -> raise Untranslatable

      | BinOp (op, e1, e2, _) -> aux_binop (Cil.typeOf e) op e1 e2

      |  Info _ -> assert false
    and aux_binop typ_res op e1 e2 =
      let g = match op with
        | PlusA -> Gauge.add (aux e1) (aux e2)
        | Mult -> Gauge.mul (aux e1) (aux e2)
        | MinusA -> Gauge.sub (aux e1) (aux e2)
        | PlusPI | IndexPI ->
          Gauge.add (aux e1) (Gauge.mul_ct (ptr_size e1) (aux e2))
        | MinusPI ->
          Gauge.add (aux e1) (Gauge.neg (Gauge.mul_ct (ptr_size e1) (aux e2)))
        | Mod | Lt | Gt | Le | Ge | Eq | Ne | BAnd | BXor | BOr | LAnd | LOr
        | MinusPP | Shiftlt | Shiftrt | Div ->
          raise Untranslatable
      in
      fits_in_type typ_res g
    (* This function also translates an expression as a gauge, but it also
       performs an on-the-fly linearization if the precise translation fails.
       Notice that this function may still raise [Untranslatable], in case
       the collaboratively computed value cannot be represented (floating-point,
       multiple pointers.). *)
    and aux e =
      try aux_gauge e
      with Untranslatable -> Gauge.ct (to_v e)
    (* Returns [g] if its evaluation fits into [typ], or raise [Untranslatable].
       If [is_cast] is false, assumes the operation is not a cast, in which
       case overflows that raise alarms are not considered as overflowing. *)
    and fits_in_type ?(is_cast=false) typ g =
      let open Eval_typ in
      match classify_as_scalar typ with
      | TSNotScalar | TSFloat _ -> raise Untranslatable
      | TSInt ir | TSPtr ir ->
        if not is_cast &&
           ((ir.i_signed && Kernel.SignedOverflow.get ()) ||
            (not ir.i_signed && Kernel.UnsignedOverflow.get ()))
        then g (* Overflows are checked for this operation *)
        else
          let v = eval_gauge state g in
          try
            let b, i = Cvalue.V.find_lonely_key v in
            if Base.equal Base.null b then
              let min = range_lower_bound ir in
              let max = range_upper_bound ir in
              let range = Ival.inject_range (Some min) (Some max) in
              if Ival.is_included i range then g else raise Untranslatable
            else
              g (* we consider pointers offsets never overflow *)
          with Not_found -> assert false (* invariant 2.1 of MV *)
    in
    aux e

  (* Store the gauge [g] for [b] in [state]. Recursively update the different
     maps until [g] is empty, then set all coefficients to 0. *)
  let store_gauge b (g: Gauge.t) (state: t): t =
    let (ct, l) = state in
    let (ctg, lg) = g in
    let rec aux l lg =
      match l, lg with
      | [], [] -> []
      | [], _ :: _ -> assert false (* impossible by construction *)
      | _, [] ->
        (* TODO: once 0 is not stored anymore in coeffs, just remove the key
           from the map. *)
        aux l [Bounds.zero]
      | (stmt, i) :: ql, cb :: qlg ->
        match i with
        | PreciseIteration _ ->
          assert (Bounds.equal cb Bounds.zero);
          (stmt, i) :: aux ql qlg
        | MultipleIterations m ->
          let coeffs = MC.add b cb m.coeffs in
          (stmt, MultipleIterations {m with coeffs}) :: aux ql qlg
    in
    let ct' = MV.add b ctg ct in
    let l' = aux l lg in
    (ct', l')
  
  let assign to_loc to_v lv e state =
    let loc = to_loc lv in
    try
      let b = loc_to_base loc (Cil.typeOfLval lv) in
      let g = translate_exp state to_loc to_v e in
      store_gauge b g state
    with Untranslatable ->
      try
        Locations.Location_Bits.fold_topset_ok
          (fun b _ state -> kill_base b state) loc.Locations.loc state
      with Abstract_interp.Error_Top -> top state

end

let dkey = Value_parameters.register_category "d-gauges"

module D_Impl : Abstract_domain.S_with_Structure
  with type state = G.t
   and type value = Cvalue.V.t
   and type location = Precise_locs.precise_location
= struct
  type value = Cvalue.V.t
  type state = G.t
  type location = Precise_locs.precise_location

  include G

  let name = "Gauges domain"

  let structure = Abstract_domain.Void
  let log_category = dkey

  let empty _ = G.empty

  let pretty = G.pretty

  let enter_scope _kf _vars state =
    state (* default is Top, nothing to do *)

  let remove_variables vars (state:state) =
    let remove_variable state v = G.kill_base (Base.of_varinfo v) state in
    List.fold_left remove_variable state vars

  let leave_scope _kf vars state =
    (* reverts implicitly to Top *)
    remove_variables vars state


  type origin = unit

  let approximate_call kf state =
    let post_state =
      let name = Kernel_function.get_name kf in
      if Ast_info.is_frama_c_builtin name ||
         (name <> "free" && Eval_typ.kf_assigns_only_result_or_volatile kf)
      then state
      else
        match !function_calls_handling with
        | FullInterprocedural -> G.top state
        | IntraproceduralAll -> state (* unsound here *)
        | IntraproceduralNonReferenced -> state
    in
    `Value [ post_state ]

  let kill loc state =
    let loc = Precise_locs.imprecise_location loc in
    let loc = loc.Locations.loc in
    let aux_base b _ acc =
      try Base.to_varinfo b :: acc
      with Base.Not_a_C_variable (* NULL *) -> acc
    in
    let vars = Locations.Location_Bits.fold_topset_ok aux_base loc [] in
    remove_variables vars state

  module Transfer (Valuation:
                     Abstract_domain.Valuation with type value = value
                                                and type origin = origin
                                                and type loc = location)
    : Abstract_domain.Transfer
      with type state := state
       and type value := value
       and type location := location
       and type valuation := Valuation.t
  = struct

    let update _valuation st = st (* TODO? *)

    exception Unassignable
    
    let assign _kinstr lv e _assignment valuation (state:state) =
      let to_loc lv =
        match Valuation.find_loc valuation lv with
        | `Value r -> Precise_locs.imprecise_location r.loc
        | `Top -> raise Unassignable
      in
      let to_val e =
        match Valuation.find valuation e with
        | `Top -> raise Unassignable
        | `Value v ->
          match v.value.initialized, v.value.escaping, v.value.v with
          | true, false, `Value v -> v
          | _ -> raise Unassignable
      in
      try `Value (G.assign to_loc to_val lv.lval e state)
      with Unassignable -> `Value (kill lv.lloc state)

    let assume_exp valuation e r state =
      if r.reductness = Created || r.reductness = Reduced then
        match e.enode with
        | Lval lv -> begin
            match Valuation.find_loc valuation lv with
            | `Top -> state
            | `Value {loc} ->
              let loc = Precise_locs.imprecise_location loc in
              try
                let b = loc_to_base loc (Cil.typeOfLval lv) in
                match r.value.v with
                | `Bottom -> state
                | `Value v ->
                  match backward_loop state b v with
                  | Some state -> state
                  | None -> state
              with Untranslatable -> state
          end
        | _ -> state
      else state

    let assume _ _ _ valuation state =
      `Value (Valuation.fold (assume_exp valuation) valuation state)

    let finalize_call _stmt _call ~pre ~post =
      let state =
        match !function_calls_handling with
        | FullInterprocedural -> post
        | IntraproceduralNonReferenced -> pre
        | IntraproceduralAll -> pre (* unsound here *)
      in
      `Value state

    let start_call _stmt call valuation state =
      let state =
        match !function_calls_handling with
        | FullInterprocedural -> update valuation state
        | IntraproceduralAll
        | IntraproceduralNonReferenced -> G.empty
      in
      (* track [arg.formal] into [state]. Important for functions that
         receive a size as argument. *)
      let aux_arg state arg =
        try
          let vi = arg.formal in
          if not (tracked_variable vi) then raise Untranslatable;
          let b = Base.of_varinfo vi in
          let v = match arg.avalue with
            | Assign v -> v
            | Copy (_, v) ->
              match v.initialized, v.escaping, v.v with
              | true, false, `Value v -> v
              | _ -> raise Untranslatable
          in
          let g = Gauge.ct v in
          store_gauge b g state
        with Untranslatable -> state
      in
      let state = List.fold_left aux_arg state call.arguments in
      Compute state

    let approximate_call _stmt call state = approximate_call call.kf state

    let show_expr _valuation _state _fmt _expr = ()
  end

  let enter_loop = G.enter_loop
  let incr_loop_counter _ = G.inc
  let leave_loop = G.leave_loop

  (* TODO: it would be interesting to return something here, but we
     currently need a valuation to perform the translation. *) 
  let extract_expr _oracle _state _exp =
    `Value (Cvalue.V.top, ()), Alarmset.all

  let extract_lval _oracle state _lv typ loc =
    let v =
      try
        let b = loc_to_base (Precise_locs.imprecise_location loc) typ in
        match extract_gauge state b with
        | Some g -> eval_gauge state g
        | None -> Cvalue.V.top
      with Untranslatable -> Cvalue.V.top
    in
    (* We can probably return an empty set of alarms when the value is known,
       but the only possible alarms on lvalues are about indeterminateness,
       and it is not clear that we know more than the Cvalue domain. *)
    `Value (v, ()), Alarmset.all

  let backward_location _state _lval _typ loc value = `Value (loc, value)

  let reduce_further _state _expr _value = []

  (* Memexec *)
  let filter_by_bases _bases state = state

  let reuse ~current_input:_ ~previous_output = previous_output

  (* Initial state *)
  let introduce_globals _ state = state
  let initialize_variable_using_type _ _ state = state
  let initialize_variable _ _ ~initialized:_ _ state = state

  (* Logic *)
  let logic_assign _assigns location ~pre:_ state = kill location state
  let evaluate_predicate _ _ _ = Alarmset.Unknown
  let reduce_by_predicate _ state _ _ = `Value state

  let top = G.empty (* must not be used, not neutral w.r.t. join (because
                       join crashes...)!! *)

end

module D = struct
  include D_Impl
  module Store = Domain_store.Make (struct
      include D_Impl
      let storage = Value_parameters.GaugesStorage.get
    end)
end
