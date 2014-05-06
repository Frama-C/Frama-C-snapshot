(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open Abstract_interp
open Locations
open CilE
open Cil_types

module V = struct

  include Location_Bytes

  exception Not_based_on_null

  let project_ival m =
    try
      let k, v = find_lonely_key m in
      if not (Base.is_null k)
      then raise Not_based_on_null
      else v
    with Not_found -> raise Not_based_on_null
	
  let min_and_max_float f =
    try
      let i = project_ival f in
      Ival.min_and_max_float i
    with Not_based_on_null -> assert false

  (* [JS 2013/01/09] unused right now *)
  let _force_float kind v =
    try
      let i = project_ival v in
      let f, fi = Ival.force_float kind i in
      f, inject_ival (fi)
    with Not_based_on_null ->
      true, topify_arith_origin v

  let is_imprecise v =
    match v with
      | Top _ -> true
      | _ -> false

  let is_topint v = equal top_int v 

  let is_bottom v = equal bottom v


  let is_isotropic v =
    match v with
      | Top _ -> true
      | Map _ -> is_topint v || is_bottom v || is_zero v

  let contains_zero loc =
    try
      let is_valid_offset base offset =
        match base with
          Base.Null ->
            if Ival.contains_zero offset then raise Base.Not_valid_offset
        | _ ->
            let bits_offset = Ival.scale (Bit_utils.sizeofchar()) offset in
            Base.is_valid_offset ~for_writing:false Int.zero base bits_offset
      in
      match loc with
      | Location_Bytes.Top _ -> true
      | Location_Bytes.Map m ->
          Location_Bytes.M.iter is_valid_offset m;
          false
    with
    | Base.Not_valid_offset -> true

  let contains_non_zero v =
    not ((equal v bottom) || (is_zero v))

  let of_char c = inject_ival (Ival.of_int (Char.code c))

  let of_int64 i = inject_ival (Ival.of_int64 i)

  let subdiv_float_interval ~size v =
    try
      let v_ival = project_ival v in
      let ival1, ival2 = 
	Ival.subdiv_float_interval ~size v_ival 
      in
      inject_ival ival1, inject_ival ival2
    with Not_based_on_null -> assert false

  let compare_bound ival_compare_bound l1 l2 =
    if l1 == l2 then 0
    else if is_bottom l2 then 1
    else if is_bottom l1 then -1
    else try
	let f1 = project_ival l1 in
	let f2 = project_ival l2 in
	ival_compare_bound f1 f2
    with Not_based_on_null -> assert false

  let compare_min_float = compare_bound Ival.compare_min_float
  let compare_max_float = compare_bound Ival.compare_max_float
  let compare_min_int = compare_bound Ival.compare_min_int
  let compare_max_int = compare_bound Ival.compare_max_int

  let filter_comparison ival_filter e1 ~cond_expr =
    let r =
    match e1 with
      | Top _  -> e1
      | Map m1 ->
          try
            let k,v2 = find_lonely_key cond_expr in
            let v1 = find_or_bottom k m1 in
            let r = Map (add_or_bottom k (ival_filter v1 v2) m1) in
            if (not (Base.equal k Base.null)) &&
              (ival_filter == Ival.filter_ge || ival_filter == Ival.filter_gt)
            then diff_if_one r singleton_zero
            else r
          with Not_found -> e1
    in
(*    Format.printf "filter_comparison %a %a -> %a@."
      pretty e1 pretty cond_expr pretty r; *)
    r

  let filter_comparison_float float_filter e1 ~cond_expr =
    try
      let v1 = project_ival e1 in
      let v2 = project_ival cond_expr in
      inject_ival (float_filter v1 v2)
    with Not_based_on_null -> e1

  let filter_le e1 ~cond_expr = filter_comparison Ival.filter_le e1 ~cond_expr
  let filter_ge e1 ~cond_expr = filter_comparison Ival.filter_ge e1 ~cond_expr
  let filter_lt e1 ~cond_expr = filter_comparison Ival.filter_lt e1 ~cond_expr
  let filter_gt e1 ~cond_expr = filter_comparison Ival.filter_gt e1 ~cond_expr

  let filter_le_float allmodes ~typ_loc e1 ~cond_expr =
    filter_comparison_float 
      (Ival.filter_le_float allmodes ~typ_loc)
      e1
      ~cond_expr
  let filter_ge_float allmodes ~typ_loc e1 ~cond_expr =
    filter_comparison_float
      (Ival.filter_ge_float allmodes ~typ_loc)
      e1 ~cond_expr
  let filter_lt_float allmodes ~typ_loc e1 ~cond_expr =
    filter_comparison_float
      (Ival.filter_lt_float allmodes ~typ_loc)
      e1
      ~cond_expr
  let filter_gt_float allmodes ~typ_loc e1 ~cond_expr =
    filter_comparison_float
      (Ival.filter_gt_float allmodes ~typ_loc)
      e1
      ~cond_expr

  let pretty fmt v =
    (*Format.printf "@[HERE@.@]";*)
    let pretty_org fmt org =
      if not (Origin.is_top org) then
        Format.fprintf fmt "@ @[(origin: %a)@]" Origin.pretty org
    in
    match v with
    | Top (Base.SetLattice.Top, a) ->
      Format.fprintf fmt "{{ ANYTHING%a }}"
        pretty_org a
    | Top (t, a) ->
      Format.fprintf fmt "{{ garbled mix of &%a%a }}"
        Base.SetLattice.pretty t
        pretty_org a
    | Map m ->
      try
        Ival.pretty fmt (project_ival v)
      with
      | Not_based_on_null ->
        let print_binding fmt k v =
          if Ival.equal Ival.singleton_zero v
          then Format.fprintf fmt "@[%a@]" Base.pretty_addr k
          else begin
	    if true
	    then
	      Format.fprintf fmt "@[%a +@ %a@]"
		Base.pretty_addr k Ival.pretty v
	    else
	      match v, Base.typeof k with
		Ival.Top _, _ 
	      | Ival.Float _, _
	      | _, None 
	      | _, Some TArray(TInt((IChar|ISChar|IUChar),_),_,_,_) -> 
		Format.fprintf fmt "@[%a +@ %a@]"
		  Base.pretty_addr k Ival.pretty v
	      | Ival.Set s, Some typ ->
		Format.fprintf fmt "@[%a +@ {"
		  Base.pretty_addr k; 
		Array.iter 
		  (fun i ->
		    Format.fprintf fmt "%a@ ("			    
		      Abstract_interp.Int.pretty i;
		    let ibits = Integer.mul (Bit_utils.sizeofchar()) i in
		    Bit_utils.pretty_offset typ ibits fmt;
		    Format.fprintf fmt "),@ ")
		  s;
		Format.fprintf fmt "}@]"
		  
		  (*		      Format.fprintf fmt "@[%a +@ %a (%a)@]"
				      Base.pretty_addr k Ival.pretty v

		  *)
	  end
        in
        Pretty_utils.pp_iter
          ~pre:"@[<hov 3>{{ " ~suf:" }}@]" ~sep:" ;@ "
          (fun pp map -> M.iter (fun k v -> pp (k, v)) map)
          (fun fmt (k, v) -> print_binding fmt k v)
          fmt m

  let inject_int (v:Int.t) =
    inject_ival (Ival.inject_singleton v)

  let interp_boolean ~contains_zero ~contains_non_zero =
    match contains_zero, contains_non_zero with
    | true, true -> zero_or_one
    | true, false -> singleton_zero
    | false, true -> singleton_one
    | false, false -> bottom

  let add v1 v2 =
    try
      Location_Bytes.shift (project_ival v1) v2
    with Not_based_on_null  ->
      try
        Location_Bytes.shift (project_ival v2) v1
      with
          Not_based_on_null  ->
            join
              (topify_arith_origin v1)
              (topify_arith_origin v2)

  (* Compute the pointwise difference between two Locations_Bytes.t. *)
  let sub_untyped_pointwise v1 v2 =
    let open Locations in
    match v1, v2 with
    | Top _, Top _
    | Top (Base.SetLattice.Top, _), Map _
    | Map _, Top (Base.SetLattice.Top, _) ->
      Ival.top, true
    | Top (Base.SetLattice.Set s, _), Map m
    | Map m, Location_Bytes.Top (Base.SetLattice.Set s, _) ->
      (* Differences between pointers containing garbled mixes must always
       result in an alarm, as garbled mix at least contain a pointer and NULL *)
      let s' = Base.SetLattice.O.add Base.null s in
      if Base.SetLattice.O.(intersects s' (from_shape (M.shape m))) then
        Ival.top, true
      else
        Ival.bottom, true
    | Map m1, Map m2 ->
      (* Substract pointwise for all the bases that are present in both m1
         and m2. Could be written more efficiently with a recursive simultaneous
         descent, but not such iterator currently exists. *)
      let aux b offsm1 (acc_offs, cardm1) =
        let acc_offs =
          try
            let offsm2 = M.find b m2 in
            Ival.join (Ival.sub offsm1 offsm2) acc_offs
          with Not_found -> acc_offs
        in
        acc_offs, succ cardm1
      in
      let offsets, cardm1 = M.fold aux m1 (Ival.bottom, 0) in
      (* If cardm1 > 1 or cardm2 > 1 or m1 and m2 are disjoint, we must emit
         an alarm *)
      let warn = cardm1 > 1 || Ival.is_bottom offsets ||
        (try ignore (find_lonely_key v2); false with Not_found -> true)
      in
      offsets, warn

  (* compute [e1+factor*e2] using C semantic for +, i.e.
     [ptr+v] is [add_untyped sizeof_in_octets( *ptr) ptr v] *)
  let add_untyped factor e1 e2 =
    try
      if Int_Base.equal factor (Int_Base.minus_one)
      then
        (* Either e1 and e2 have the same base, and it's a subtraction
           of pointers, or e2 is really an integer *)
        let b1, o1 = Location_Bytes.find_lonely_key e1 in
        let b2, o2 = Location_Bytes.find_lonely_key e2 in
        if Base.compare b1 b2 <> 0 then raise Not_found;
        inject_ival (Ival.sub o1 o2)
      else begin
        if not (Int_Base.equal factor (Int_Base.one))
        then raise Not_found; (* cannot multiply a pointer *)
        add e1 e2
      end
    with Not_found ->
      (* we end up here if the only way left to make this
         addition is to convert e2 to an integer *)
      try
        let right = Ival.scale_int_base factor (project_ival e2)
        in Location_Bytes.shift right e1
      with Not_based_on_null  -> (* from [project_ival] *)
        join (topify_arith_origin e1) (topify_arith_origin e2)

  let compare_min_max min max =
    match min, max with
    | None,_ -> -1
    | _,None -> -1
    | Some min, Some max -> Int.compare min max

  let compare_max_min max min =
    match max, min with
    | None,_ -> 1
    | _,None -> 1
    | Some max, Some min -> Int.compare max min

  let do_le min1 max1 min2 max2 =
    if compare_max_min max1 min2 <= 0 then singleton_one
    else if compare_min_max min1 max2 > 0 then singleton_zero
    else zero_or_one

  let do_ge min1 max1 min2 max2 =
    do_le min2 max2 min1 max1

  let do_lt min1 max1 min2 max2 =
    if compare_max_min max1 min2 < 0 then singleton_one
    else if compare_min_max min1 max2 >= 0 then singleton_zero
    else zero_or_one

  let do_gt min1 max1 min2 max2 =
    do_lt min2 max2 min1 max1

  let _comparisons _info ~signed f e1 e2 =
    let r =
    try
      let k1,v1 = find_lonely_key e1 in
      let k2,v2 = find_lonely_key e2 in
      if not (Base.equal k1 k2)
      then begin
          if (not signed)
          then begin
              let e1_zero = equal e1 singleton_zero in
              let e2_zero = equal e2 singleton_zero in
              if (e1_zero && (f == do_le || f == do_lt))
                || (e2_zero && (f == do_ge || f == do_gt))
              then singleton_one
              else if (e2_zero && (f == do_le || f == do_lt))
                || (e1_zero && (f == do_ge || f == do_gt))
              then singleton_zero
              else zero_or_one
            end
          else zero_or_one
        end
      else Ival.compare_C f v1 v2
    with Not_found ->
      zero_or_one
    in
(*    Format.printf "comparisons %a %a %a@."
      pretty e1 pretty e2 pretty r; *)
    r

  let asym_rel ~signed op e1 e2 = 
    let open Cil_types in
    try
      let k1,v1 = find_lonely_key e1 in
      let k2,v2 = find_lonely_key e2 in
      if Base.equal k1 k2 then begin
        let f = match op with
          | Ge -> do_ge
          | Le -> do_le
          | Gt -> do_gt
          | Lt -> do_lt
          | _ -> assert false
        in
        Ival.compare_C f v1 v2
      end else begin
        if signed then
          zero_or_one
        else begin
          let e1_zero = equal e1 singleton_zero in
          let e2_zero = equal e2 singleton_zero in
          if (e1_zero && (op = Le || op = Lt))
          || (e2_zero && (op = Ge || op = Gt))
          then singleton_one
          else
            if (e2_zero && (op = Le || op = Lt))
            || (e1_zero && (op = Ge || op = Gt))
            then singleton_zero
            else zero_or_one
        end
      end
    with Not_found -> zero_or_one


  let check_equal positive e1 e2 =
    let one,zero =
      if positive
      then Ival.singleton_one,  Ival.singleton_zero
      else Ival.singleton_zero, Ival.singleton_one
    in
    inject_ival
      (if (equal e1 e2) && (cardinal_zero_or_one e1)
       then one
       else
         if intersects e1 e2
         then Ival.zero_or_one
         else zero)

  let eval_comp ~signed op v1 v2 =
    let open Cil_types in
    match op with
      | Eq -> check_equal true v1 v2
      | Ne -> check_equal false v1 v2
      | Le | Ge | Lt | Gt -> asym_rel ~signed op v1 v2
      | _ -> assert false


  let cast_float ~rounding_mode v =
    try
      let i = project_ival v in
      let b, i = Ival.cast_float ~rounding_mode i in
      false, b, inject_ival i
    with
      Not_based_on_null ->
	if is_bottom v
	then false, false, bottom
	else true, true, topify_arith_origin v

  let cast_double v =
    try
      let i = project_ival v in
      let b, i = Ival.cast_double i in
      false, b, inject_ival i
    with
      Not_based_on_null ->
	if is_bottom v
	then false, false, bottom
	else true, true, topify_arith_origin v

  let cast ~size ~signed expr =
    try
      let i = project_ival expr in
      inject_ival (Ival.cast ~size ~signed ~value:i), true
    with
      | Not_based_on_null ->
          if Int.ge size (Int.of_int (Bit_utils.sizeofpointer ())) ||
            is_bottom expr || is_imprecise expr
          then expr, true
          else topify_arith_origin expr, false

  let import_function ~topify ~with_alarms info f e1 e2 =
    try
      let v1 = project_ival e1 in
      let v2 = project_ival e2 in
      inject_ival (f v1 v2)
    with Not_based_on_null  ->
      if is_bottom e1 || is_bottom e2 
      then bottom
      else begin
	(do_warn with_alarms.imprecision_tracing
	   (fun _ ->
             match e1,e2 with
               | Map _, Map _ ->
		 Kernel.warning ~once:true ~current:true
		   "Operation %a %s %a incurs a loss of precision"
		   pretty e1
		   info
		   pretty e2
               | _ -> ()));
	  join
            (topify_with_origin_kind topify e1)
            (topify_with_origin_kind topify e2)
	end

  let arithmetic_function = import_function ~topify:Origin.K_Arith

 let cast_float_to_int ~signed ~size v =
   try
     let v1 = project_ival v in
     let alarm_use_as_float, alarm_overflow, r =
       Ival.cast_float_to_int ~signed ~size v1
     in
     false, alarm_use_as_float, alarm_overflow, inject_ival r
   with Not_based_on_null ->
     (not (is_bottom v)), true, true, topify_arith_origin v

 let cast_float_to_int_inverse ~single_precision i =
   try
     let v1 = project_ival i in
     let r = Ival.cast_float_to_int_inverse ~single_precision v1
     in
     inject_ival r
   with Not_based_on_null -> assert false

 let cast_int_to_float rounding_mode v =
   try
     let i = project_ival v in
     let ok, r = Ival.cast_int_to_float rounding_mode i in
     inject_ival r, ok
   with Not_based_on_null -> v, false

  let div ~with_alarms e1 e2 =
    if equal e2 singleton_one
    then e1
    else begin
      if (with_alarms.others.a_log <> None) && contains_zero e2 then
	CilE.warn_div with_alarms;
      arithmetic_function ~with_alarms "/" Ival.div e1 e2
    end

  let c_rem ~with_alarms e1 e2 =
    if (with_alarms.others.a_log <> None) && contains_zero e2 then
      warn_div with_alarms;
    arithmetic_function ~with_alarms "%" Ival.c_rem e1 e2

  let mul ~with_alarms e1 e2 =
    arithmetic_function ~with_alarms "*" Ival.mul e1 e2

  (** Warn about overflow iff [size] is not [None]. Beware when calling
      this function *)
  let shift_left ~topify ~with_alarms ~size e1 e2 =
    let default e1 e2 = 
          begin
            try
              let size = 
		Extlib.opt_map 
		  (function (_, y) -> Int.of_int y)
		  size
	      in
              import_function
                ~topify
                ~with_alarms
                "<<"
                (Ival.shift_left ~size) e1 e2
            with Not_found ->
              join
                (topify_with_origin_kind topify e1)
                (topify_with_origin_kind topify e2)
          end
    in
    match size with
      | None -> default e1 e2
      | Some ((warn_negative, size)) ->
          let size_int = Int.of_int size in
          let valid_range_rhs =
            inject_ival 
	      (Ival.inject_range 
		  (Some Int.zero)
                  (Some (Int.pred size_int))) 
	  in
          if (with_alarms.others.a_log <> None)
	  then begin
              if not (is_included e2 valid_range_rhs)
              then warn_shift with_alarms size;
	    end;
	  let e2 = narrow e2 valid_range_rhs in
	  let e1 =
	    if warn_negative
	    then begin
		let valid_range_lhs = 
		  inject_ival 
		    (Ival.inject_range 
			(Some Int.zero)
			None)
		in
		if not (is_included e1 valid_range_lhs)
		then warn_shift_left_positive with_alarms;
		narrow e1 valid_range_lhs
	      end
	    else e1
	  in
	  default e1 e2

  let bitwise_xor ~with_alarms v1 v2 =
    arithmetic_function ~with_alarms "^" Ival.bitwise_xor v1 v2

  let bitwise_or_with_topify  ~topify ~with_alarms v1 v2 =
    import_function ~topify ~with_alarms "^" Ival.bitwise_or v1 v2


  let shift_right ~with_alarms ~size e1 e2 =
    let default () = 
          begin
            try
              let size = Extlib.opt_map (function (_,s) -> Int.of_int s) size in
              arithmetic_function ~with_alarms ">>"
                (Ival.shift_right ~size) e1 e2
            with Not_found ->
              join (topify_arith_origin e1) (topify_arith_origin e2)
          end
    in
    match size with
      | None -> default ()
      | Some (_,size) ->
          let size_int = Int.of_int size in
          let valid_range =
            inject_ival (Ival.inject_range (Some Int.zero)
                                           (Some (Int.pred size_int)))
          in
          if not (intersects e2 valid_range) then begin
            warn_shift  with_alarms size;
            if with_alarms.others.a_log <> None then
              Kernel.warning ~once:true ~current:true
                "invalid shift of %a-bit value by %a. \
This path is assumed to be dead."
                Int.pretty size_int
                pretty e2;
            bottom
          end else begin
            if (with_alarms.others.a_log <> None)
              && not (is_included e2 valid_range)
            then warn_shift with_alarms size;
            default ()
          end

  let bitwise_and ~signed ~size e1 e2 =
    let bitwise_and_pointer_ival p ival =
      let _min, _max =
	match Ival.min_and_max ival with
	  Some min, Some max when Int.ge min Int.zero ->
	    min, max
	| _ -> raise Not_based_on_null
      in
      let treat_base _base _offsets _acc =
	Location_Bytes.topify_arith_origin p (* TODO *)
      in
      Location_Bytes.fold_i treat_base p Location_Bytes.bottom
    in
      try
        let v1 = project_ival e1 in
        try
          let v2 = project_ival e2 in
          let result = Ival.bitwise_and ~signed ~size v1 v2
          in
          inject_ival result
        with Not_based_on_null | Location_Bytes.Error_Top ->
          bitwise_and_pointer_ival e2 v1
      with Not_based_on_null | Location_Bytes.Error_Top ->
        try
          let v2 = project_ival e2 in
          bitwise_and_pointer_ival e1 v2
        with Not_based_on_null | Location_Bytes.Error_Top ->
          join (topify_arith_origin e1) (topify_arith_origin e2)

  let extract_bits ~topify ~start ~stop ~size v =
    try
      let i = project_ival v in
      false, inject_ival (Ival.extract_bits ~start ~stop ~size i)
    with
      | Not_based_on_null ->
          if is_imprecise v
          then false, v
          else true, topify_with_origin_kind topify v

  let big_endian_merge_bits ~topify ~conflate_bottom ~total_length ~length ~value ~offset acc =
    if is_bottom acc || is_bottom value
    then begin
        if conflate_bottom
        then
          bottom
        else
          join
            (topify_with_origin_kind topify acc)
            (topify_with_origin_kind topify value)
      end
    else
    let total_length_i = Int.of_int total_length in
    assert (Int.le (Int.add length offset) total_length_i);
    let result =
      bitwise_or_with_topify
        ~topify
        ~with_alarms:warn_none_mode
        (shift_left
            ~topify
            ~with_alarms:warn_none_mode
            ~size:(Some (false,total_length))
            value
           (inject_ival (Ival.inject_singleton (Int.sub (Int.sub total_length_i offset) length))))
        acc
    in
(*    Format.printf "big_endian_merge_bits : total_length:%d length:%a value:%a offset:%a acc:%a GOT:%a@."
      total_length
      Int.pretty length
      pretty value
      Int.pretty offset
      pretty acc
      pretty result; *)
    result

  let little_endian_merge_bits ~topify ~conflate_bottom ~total_length ~value
      ~offset acc =
    if is_bottom acc || is_bottom value
    then begin
        if conflate_bottom
        then
          bottom
        else
          join
            (topify_with_origin_kind topify acc)
            (topify_with_origin_kind topify value)
      end
    else
    let result =
      bitwise_or_with_topify
        ~topify
        ~with_alarms:warn_none_mode
        (shift_left
           ~topify
           ~with_alarms:warn_none_mode
           ~size:(Some (false, total_length))
           value
           (inject_ival (Ival.inject_singleton offset)))
        acc
    in
    (*Format.printf "le merge_bits : total_length:%d value:%a offset:%a acc:%a GOT:%a@."
      total_length pretty value Int.pretty offset pretty acc pretty result;*)
    result

  (* neutral value for foo_endian_merge_bits *)
  let merge_neutral_element = singleton_zero

  let all_values ~size v =
    try
      let i = project_ival v in
      Ival.all_values ~size i
    with Not_based_on_null -> 
      false

  let anisotropic_cast ~size v =
    if all_values ~size v then top_int else v

  let create_all_values ~modu ~signed ~size =
    inject_ival (Ival.create_all_values ~modu ~signed ~size)

  let bitwise_or = bitwise_or_with_topify ~topify:Origin.K_Arith
  let shift_left = shift_left ~topify:Origin.K_Arith
end

module V_Or_Uninitialized = struct

  (* Note: there is a "cartesian product" of the escape and init flags
     in the constructors, instead of having a tuple or two sum types,
     for performance reasons: this avoids an indirection. *)
  type un_t =
    | C_uninit_esc of V.t
    | C_uninit_noesc of V.t
    | C_init_esc of V.t
    | C_init_noesc of V.t

  type tt = un_t

  let mask_init = 2
  let mask_noesc = 1

  let is_initialized flags = (flags land mask_init) <> 0
  let is_noesc flags = (flags land mask_noesc) <> 0

  let get_v = function 
    | C_uninit_esc  v
    | C_uninit_noesc v
    | C_init_esc v
    | C_init_noesc v -> v

  external get_flags : tt -> int = "caml_obj_tag" "noalloc"

  let create : int -> V.t -> tt = fun flags v ->
    match flags with
    | 0 -> C_uninit_esc v
    | 1 -> C_uninit_noesc v
    | 2 -> C_init_esc v
    | 3 -> C_init_noesc v
    | _ -> assert false

(* let (==>) = (fun x y -> (not x) || y) *)

  type widen_hint = V.widen_hint
  let widen wh t1 t2 =
    create (get_flags t2) (V.widen wh (get_v t1) (get_v t2))

  let equal t1 t2 =
    (get_flags t1) = (get_flags t2) &&
    V.equal (get_v t1) (get_v t2)

  let join t1 t2 =
    create
      ((get_flags t1) land (get_flags t2))
      (V.join (get_v t1) (get_v t2))

  let narrow t1 t2 =
    create
      ((get_flags t1) lor (get_flags t2))
      (V.narrow (get_v t1) (get_v t2))

  let link t1 t2 =
    create
      ((get_flags t1) land (get_flags t2))
      (V.link (get_v t1) (get_v t2))

  let meet t1 t2 =
   create
      ((get_flags t1) lor (get_flags t2))
      (V.meet (get_v t1) (get_v t2))

  let bottom = C_init_noesc V.bottom
  let top = C_uninit_esc V.top

  let is_bottom = equal bottom

  let uninitialized = C_uninit_noesc V.bottom
  let initialized v = C_init_noesc v

  let remove_indeterminateness v =
    match v with
      C_init_noesc _ -> v
    | (C_uninit_noesc v | C_uninit_esc v | C_init_esc v) -> C_init_noesc v

  let is_included t1 t2 =
(*    (t2.initialized ==> t1.initialized) &&
    (t2.no_escaping_adr ==> t1.no_escaping_adr) &&
      V.is_included t1.v t2.v
*)
    let flags1 = get_flags t1 in
    let flags2 = get_flags t2 in
    (lnot flags2) lor flags1 = -1 &&
        V.is_included (get_v t1) (get_v t2)

  let join_and_is_included t1 t2 =
    let t12 = join t1 t2 in (t12, equal t12 t2)

  let pretty fmt t =
    let flags = get_flags t in
    let no_escaping_adr = is_noesc flags in
    let initialized = is_initialized flags in
    let v = get_v t in
    match V.(equal bottom v), initialized, no_escaping_adr with
      | false, false, false ->
        Format.fprintf fmt "%a or UNINITIALIZED or ESCAPINGADDR" V.pretty v
      | true, false, false ->
        Format.pp_print_string fmt "UNINITIALIZED or ESCAPINGADDR"
      | false, false, true ->
        Format.fprintf fmt "%a or UNINITIALIZED" V.pretty v
      | true, false, true ->
        Format.pp_print_string fmt "UNINITIALIZED"
      | false, true, false ->
        Format.fprintf fmt "%a or ESCAPINGADDR" V.pretty v
      | true, true, false ->
        Format.pp_print_string fmt "ESCAPINGADDR"
      | false, true, true ->
        V.pretty fmt v
      | true, true, true ->
        Format.pp_print_string fmt "BOTVALUE"

  let cardinal_zero_or_one t =
    match t with
      C_init_noesc v -> V.cardinal_zero_or_one v
    | C_init_esc v | C_uninit_noesc v -> V.is_bottom v
    | C_uninit_esc _ -> false

  let hash t = (get_flags t) * 4513 + (V.hash (get_v t))

  include Datatype.Make
      (struct
        type t = tt (* =     | C_uninit_esc of V.t
                       | C_uninit_noesc of V.t
                       | C_init_esc of V.t
                       | C_init_noesc of V.t *)
        let name = "Cvalue.V_Or_Uninitialized"
        let structural_descr =
	  let v = V.packed_descr in
           Structural_descr.t_sum [| [| v |]; [| v |]; [| v |]; [| v |] |]
        let reprs =
          List.fold_left
            (fun acc v ->
              List.fold_left
                (fun acc v ->
                  List.fold_left
                    (fun acc v -> C_uninit_noesc v :: acc)
                    (C_uninit_esc v :: acc)
                    V.reprs)
                (C_init_noesc v :: acc)
                V.reprs)
            (List.map (fun v -> C_init_esc v) V.reprs)
            V.reprs
        let hash = hash
        let equal = equal
        let compare = Datatype.undefined
        let copy = Datatype.undefined
        let rehash = Datatype.identity
        let pretty = pretty
        let internal_pretty_code = Datatype.undefined
        let varname = Datatype.undefined
        let mem_project = Datatype.never_any_project
       end)

  module Top_Param = Base.SetLattice

  let is_isotropic t = V.is_isotropic (get_v t)

  let cardinal_zero_or_one_or_isotropic t =
    cardinal_zero_or_one t || is_isotropic t

  let extract_bits ~topify ~start ~stop ~size t =
    let inform_extract_pointer_bits, v =
      V.extract_bits ~topify ~start ~stop ~size (get_v t)
    in
    inform_extract_pointer_bits,
    create (get_flags t) v

  let little_endian_merge_bits ~topify ~conflate_bottom ~total_length ~value ~offset t =
    create
      ((get_flags t) land (get_flags value))
      (V.little_endian_merge_bits ~topify ~conflate_bottom
          ~total_length ~value:(get_v value) ~offset
          (get_v t))

  let big_endian_merge_bits ~topify ~conflate_bottom ~total_length ~length ~value ~offset t =
    create
      ((get_flags t) land (get_flags value))
      (V.big_endian_merge_bits ~topify ~conflate_bottom
          ~total_length ~length
          ~value:(get_v value)
          ~offset
          (get_v t))

  let topify_with_origin o t =
    create
      (get_flags t)
      (V.topify_with_origin o (get_v t))

  let anisotropic_cast ~size t =
    create
      (get_flags t)
      (V.anisotropic_cast ~size (get_v t))

  let singleton_zero = C_init_noesc (V.singleton_zero)
  let merge_neutral_element = singleton_zero

  let unspecify_escaping_locals ~exact is_local t =
    let flags = get_flags t in
    let flags = flags land mask_init
      (* clear noesc flag *)
    in
    let v = get_v t in
    let locals, v' = V.remove_escaping_locals is_local v in
    let v = if exact then v' else V.join v v' in
    locals, create flags v

  let change_initialized init v = match init, v with
    | true, C_uninit_esc v -> C_init_esc v
    | true, C_uninit_noesc v -> C_init_noesc v
    | true, _ -> v
    | false, C_init_esc v -> C_uninit_esc v
    | false, C_init_noesc v -> C_uninit_noesc v
    | false, _ -> v

  let project_with_alarms ~with_alarms ~conflate_bottom loc v =
    let v_v = get_v v in
    let bottom = V.is_bottom v_v in
    let flags = get_flags v in

    (* distasteful FIXME *) if conflate_bottom then begin
        if not (is_initialized flags)
        then warn_uninitialized with_alarms;
        if not (is_noesc flags)
        then warn_escapingaddr with_alarms;
      end;

    if with_alarms.unspecified.a_log <> None &&
      bottom &&
      not (is_initialized flags &&
             is_noesc flags )
    then begin
      do_warn with_alarms.unspecified
	(fun _ ->
          Kernel.warning ~current:true ~once:true
          "completely indeterminate value %a."
            (Locations.pretty_english ~prefix:true) loc)
    end;
    v_v

end

module V_Offsetmap = struct
  include Offsetmap.Make(V_Or_Uninitialized)

  let from_string s =
    (* Iterate on s + null terminator; same signature as List.fold_left *)
    let fold_string f acc s =
      let acc = ref acc in
      for i = 0 to String.length s - 1 do
        let v = V_Or_Uninitialized.initialized (V.of_char s.[i]) in
        acc := f !acc v;
      done;
      f !acc V_Or_Uninitialized.singleton_zero (** add null terminator *)
    in
    let size_char = Integer.of_int (Cil.bitsSizeOfInt IChar) in
    of_list fold_string s size_char

  let from_wstring s =
    let conv v = V_Or_Uninitialized.initialized (V.of_int64 v) in
    let fold f acc l = List.fold_left (fun acc v -> f acc (conv v)) acc l in
    let size_wchar = Integer.of_int Cil.(bitsSizeOf theMachine.wcharType) in
    of_list fold (s @ [0L]) size_wchar

  let from_cstring = function
    | Base.CSWstring w -> from_wstring w
    | Base.CSString s -> from_string s
end

module Default_offsetmap = struct

  module InitializedVars =
    Cil_state_builder.Varinfo_hashtbl
      (V_Offsetmap)
      (struct
         let name = "Cvalue.Default_offsetmap.InitializedVars"
         let dependencies = [ Ast.self ]
         let size = 117
       end)
  let () = Ast.add_monotonic_state InitializedVars.self

  let create_initialized_var varinfo validity initinfo =
    InitializedVars.add varinfo initinfo;
    Base.register_initialized_var varinfo validity

  let default_offsetmap base = match base with
  | Base.Initialized_Var (v,_) ->
      (try InitializedVars.find v
       with Not_found -> assert false)
  | Base.Var _ | Base.CLogic_Var _ ->
      begin
        match Base.validity base with
        | Base.Invalid -> V_Offsetmap.empty
        | Base.Known (mn, mx) | Base.Unknown (mn, _, mx) ->
            assert (Int.ge mx mn && Int.equal mn Int.zero);
            V_Offsetmap.create_isotropic ~size:(Int.succ mx)
              V_Or_Uninitialized.uninitialized
        | Base.Periodic (mn, mx, p) ->
            assert (Int.is_zero mn && Int.gt mx p);
            V_Offsetmap.create_isotropic ~size:p
              V_Or_Uninitialized.bottom
      end
  | Base.Null -> V_Offsetmap.empty
  | Base.String (_,i) -> V_Offsetmap.from_cstring i
end

module Model = struct

  include
    Lmap.Make_LOffset(V_Or_Uninitialized)(V_Offsetmap)(Default_offsetmap)

  let find_unspecified = find

  let find ~with_alarms ~conflate_bottom state loc =
    let v = find_unspecified ~with_alarms ~conflate_bottom state loc in
    V_Or_Uninitialized.project_with_alarms ~with_alarms ~conflate_bottom loc v

let reduce_by_initialized_defined_loc f loc_bits size state =
  try
    let base, offset = Locations.Location_Bits.find_lonely_key loc_bits in
    let ll = Ival.project_int offset in
    let lh = Int.pred (Int.add ll size) in
    let offsm = find_base base state in
    let aux (offl, offh) (v, modu, shift) acc =
      let v' = f v in
      if v' != v then begin
        if V_Or_Uninitialized.is_bottom v' then raise Exit;
        let il = Int.max offl ll and ih = Int.min offh lh in
        let abs_shift = Integer.pos_rem (Rel.add_abs offl shift) modu in
        (* il and ih are the bounds of the interval to reduce.
           We change the initialized flags in the following cases:
           - either we overwrite entire values, or the partly overwritten
           value is at the beginning or at the end of the subrange
           - or we do not lose information on misaligned or partial values:
           the result is a singleton *)
        if V_Or_Uninitialized.cardinal_zero_or_one_or_isotropic v' ||
	  ((Int.equal offl il || Int.equal (Int.pos_rem ll modu) abs_shift) &&
           (Int.equal offh ih || Int.equal
                                   (Int.pos_rem (Int.succ lh) modu) abs_shift))
        then
          let diff = Rel.sub_abs il offl in
          let shift_il = Rel.pos_rem (Rel.sub shift diff) modu in
          V_Offsetmap.add (il, ih) (v', modu, shift_il) acc
        else acc
      end
      else acc
     in
     let noffsm =
       V_Offsetmap.fold_between ~entire:true (ll, lh) aux offsm offsm
     in
     add_base base noffsm state
   with
     | Exit -> bottom
     | Not_found (* from find_lonely_key *)
     | Ival.Not_Singleton_Int (* from Ival.project_int *) ->
         state

  let find_and_reduce_indeterminate ~with_alarms state loc =
    let conflate_bottom = true in
    let v = find_unspecified ~conflate_bottom ~with_alarms state loc in
    let v_v = 
      V_Or_Uninitialized.project_with_alarms ~with_alarms ~conflate_bottom loc v
    in
    let loc_bits = loc.Locations.loc in
    let state = 
      match v with
	| V_Or_Uninitialized.C_uninit_esc _
        | V_Or_Uninitialized.C_uninit_noesc _
        | V_Or_Uninitialized.C_init_esc _
            when Locations.cardinal_zero_or_one loc
          ->
            (* Does not raise an exception, given the definition of
               Locations.cardinal_zero_or one *)
	    let size = Int_Base.project loc.size in
	    reduce_by_initialized_defined_loc
	      V_Or_Uninitialized.remove_indeterminateness loc_bits size state
        | _ -> state
    in
    state, v_v

  let add_binding_unspecified ~exact mem loc v =
    add_binding ~reducing:false ~with_alarms:warn_none_mode ~exact mem loc v

  let reduce_previous_binding initial_mem l v =
    assert (Locations.cardinal_zero_or_one l);
    let v = V_Or_Uninitialized.initialized v in
    add_binding ~with_alarms:CilE.warn_none_mode
      ~reducing:true ~exact:true initial_mem l v

(* XXXXXXXXX bug with uninitialized values ? *)
  let reduce_binding initial_mem l v =
    let with_alarms = CilE.warn_none_mode in
    let v_old = find ~conflate_bottom:true ~with_alarms initial_mem l in
    if V.equal v v_old
    then initial_mem
    else
      let vv = V.narrow v_old v in
(* Format.printf "narrow %a %a %a@." V.pretty v_old V.pretty v V.pretty vv; *)
      if V.equal vv v_old then initial_mem
      else reduce_previous_binding initial_mem l vv
    
  let add_initial_binding mem loc v =
    add_binding ~with_alarms:warn_none_mode
      ~reducing:true ~exact:true mem loc v
    
  (* Overwrites the definition of add_binding coming from Lmap, with a
     signature change. *)
  let add_binding ~with_alarms ~exact acc loc value =
    add_binding ~with_alarms
      ~reducing:false ~exact acc loc (V_Or_Uninitialized.initialized value)

  let add_new_base base ~size v ~size_v state  =
    let v = V_Or_Uninitialized.initialized v in
    add_new_base base ~size v ~size_v state

  let uninitialize_blocks_locals blocks state =
    List.fold_left
      (fun acc block ->
        List.fold_left
          (fun acc vi -> remove_base (Base.of_varinfo vi) acc)
          acc
          block.blocals)
      state
      blocks

 let uninitialize_formals_locals fundec state =
    let locals = List.map Base.of_varinfo fundec.slocals in
    let formals = List.map Base.of_varinfo fundec.sformals in
    let cleanup acc v = remove_base v acc in
    let result = List.fold_left cleanup state locals in
    List.fold_left cleanup result formals

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
