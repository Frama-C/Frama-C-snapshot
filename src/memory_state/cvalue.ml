(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(* types of values *)
open Cil
open Abstract_interp
open Abstract_value
open Locations
open CilE

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

  let types = Hashtbl.create 7;;

  let pretty_int_range fmt print_ampamp typname lv v =
    let v = project_ival v in
    ( match Ival.min_and_max v with
      Some mn, Some mx ->
        if Int.equal mn mx
        then begin
            print_ampamp();
            Format.fprintf fmt "*(%s*)%s == %a"
              typname
              lv
              Int.pretty mn
          end
        else begin
            print_ampamp();
            Format.fprintf fmt "%a <= *(%s*)%s && *(%s*)%s <= %a"
              Int.pretty mn
              typname
              lv
              typname
              lv
              Int.pretty mx;
          end
    | _ -> ())

  let pretty_float_range fmt print_ampamp typname lv v =
    let use_hex = true in
    let v = project_ival v in
    let v = Ival.project_float v in
    let mn, mx = Ival.Float_abstract.min_and_max_float v in
        if Ival.F.equal mn mx
        then begin
            print_ampamp();
            Format.fprintf fmt "*(%s*)%s == %a"
              typname
              lv
              (Ival.F.pretty_normal ~use_hex) mn
          end
        else begin
            print_ampamp();
            Format.fprintf fmt "%a <= *(%s*)%s && *(%s*)%s <= %a"
              (Ival.F.pretty_normal ~use_hex) mn
              typname
              lv
              typname
              lv
              (Ival.F.pretty_normal ~use_hex) mx;
          end

  let () =
    Hashtbl.add types 1
      [inject_ival (Ival.inject_range
                       (Some Int.zero) (Some (Int.of_int 255))),
      "unsigned char", pretty_int_range;
       inject_ival (Ival.inject_range
                       (Some (Int.of_int (-128))) (Some (Int.of_int 127))),
      "char", pretty_int_range];
    Hashtbl.add types 2
      [inject_ival (Ival.inject_range
                       (Some Int.zero) (Some (Int.of_int 65535))),
      "unsigned short", pretty_int_range;
       inject_ival (Ival.inject_range
                       (Some (Int.of_int (-32768))) (Some (Int.of_int 32767))),
      "short", pretty_int_range];
    Hashtbl.add types 4
      [ top_float,
      "float", pretty_float_range;
        inject_ival (Ival.inject_range
                        (Some Int.zero) (Some (Int.of_string "4294967295"))),
      "unsigned int", pretty_int_range;
        inject_ival (Ival.inject_range
                        (Some (Int.of_string "-2147483648"))
                        (Some (Int.of_string  "2147483647"))),
      "int", pretty_int_range];
    Hashtbl.add types 8
      [ top_float,
      "double", pretty_float_range];
    ()

  let pretty_c_assert print_ampamp lv s_bytes fmt v =
    try
      let candidate_types = Hashtbl.find types s_bytes in
      let rec find_typ l =
        match l with
          [] -> ()
        | (range, _, _) :: t when not (is_included v range) ->
            find_typ t
        | (_range, typname, pr) :: _ ->
            pr fmt print_ampamp typname lv v

      in
      find_typ candidate_types
    with Not_based_on_null -> ()

  let force_float kind v =
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
    | Int_Base.Error_Top | Int_Base.Error_Bottom
    | Base.Not_valid_offset -> true

  let contains_non_zero v =
    not ((equal v bottom) || (is_zero v))

  let of_char c = inject_ival (Ival.of_int (Char.code c))

  let of_int64 i = inject_ival (Ival.of_int64 i)

  let subdiv_float_interval ~size v =
    try
      let v_ival = project_ival v in
      let ival1, ival2 = Ival.subdiv_float_interval ~size v_ival in
      inject_ival ival1, inject_ival ival2
    with Not_based_on_null -> assert false

  let compare_bound ival_compare_bound l1 l2 =
    try
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
      | Top (Top_Param.Top, a) ->
          Format.fprintf fmt "{{ ANYTHING%a }}"
            pretty_org a
      | Top (t, a) ->
          Format.fprintf fmt "{{ garbled mix of &%a%a }}"
            Top_Param.pretty t
            pretty_org a
      | Map m ->
          try
            Ival.pretty fmt (project_ival v)
          with
            | Not_based_on_null ->
                let print_binding fmt k v =
                  if Ival.equal Ival.singleton_zero v
                  then Format.fprintf fmt "@[&%a@]" Base.pretty k
                  else
                    Format.fprintf fmt "@[&%a +@ %a@]"
                      Base.pretty k Ival.pretty v
                in
                Pretty_utils.pp_iter
                  ~pre:"@[<hov 3>{{ " ~suf:" }}@]" ~sep:" ;@ "
                  (fun pp map -> M.iter (fun k v -> pp (k, v)) map)
                  (fun fmt (k, v) -> print_binding fmt k v)
                  fmt m

(* Returns the list of non NULL Base.t that have a null offset in the map.
   The boolean is true iff the result is exact. Otherwise it is
   under-approximated. *)
  let find_exact_base_without_offset v = match v with
    | Top _ -> [],false
    | Map m ->
        let exact = ref true in
        let result = ref [] in
        let get_binding k v =
          if Ival.equal Ival.singleton_zero v
          then result := k::!result
          else exact := false
        in
        M.iter get_binding m;
        !result,!exact

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
      Location_Bytes.location_shift (project_ival v1) v2
    with Not_based_on_null  ->
      try
        Location_Bytes.location_shift (project_ival v2) v1
      with
          Not_based_on_null  ->
            join
              (topify_arith_origin v1)
              (topify_arith_origin v2)

  (* compute [e1+factor*e2] using C semantic for +, i.e.
     [ptr+v] is [add_untyped sizeof_in_octets( *ptr) ptr v] *)
  let add_untyped factor e1 e2 =
    try
      if Int_Base.equal factor (Int_Base.minus_one)
      then
        (* Either e1 and e2 have the same base, and it's a substraction
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
        let right = Ival.scale_int64base factor (project_ival e2)
        in Location_Bytes.location_shift right e1
      with Not_based_on_null  -> (* from [project_ival] *)
        join (topify_arith_origin e1) (topify_arith_origin e2)

  let rec check_equal positive e1 e2 =
    let one,zero =
      if positive then Ival.singleton_one, Ival.singleton_zero else
        Ival.singleton_zero, Ival.singleton_one
    in
    inject_ival
      (if (equal e1 e2) && (cardinal_zero_or_one e1)
       then one
       else
         if intersects e1 e2
         then Ival.zero_or_one
         else zero)

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

  let comparisons _info ~signed f e1 e2 =
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


  let cast_float v =
    try
      let i = project_ival v in
      let b, i = Ival.cast_float i in
      false, b, inject_ival i
    with
      Not_based_on_null ->
        true, true, topify_arith_origin v

  let cast ~with_alarms ~size ~signed expr =
    try
      let i = project_ival expr in
        inject_ival (Ival.cast ~size ~signed ~value:i)
    with
      | Not_based_on_null ->
          if Int.compare size (Int.of_int (Bit_utils.sizeofpointer ())) >= 0
            || (match expr with Top _ -> true | _ -> false)
          then expr
          else begin
             if is_bottom expr || is_imprecise expr then expr
             else begin
                (match with_alarms.imprecision_tracing with
                 | Aignore -> ()
                 | Acall f -> f ()
                 | Alog _ ->
                   Kernel.warning ~once:true ~current:true
                     "casting address to a type smaller than sizeof(void*): \
@[%a@]"
                       Location_Bytes.pretty expr);
               topify_arith_origin expr
             end
          end

  let import_function ~topify_arith_origin ~with_alarms info f e1 e2 =
    try
      let v1 = project_ival e1 in
      let v2 = project_ival e2 in
      inject_ival (f v1 v2)
    with Not_based_on_null  ->
      (match with_alarms.imprecision_tracing with
       | Aignore -> ()
       | Acall f -> f ()
       | Alog _ ->
           match e1,e2 with
           | Map _, Map _ ->
             Kernel.warning ~once:true ~current:true
               "Operation %a %s %a incurs a loss of precision"
               pretty e1
               info
               pretty e2
           | _ -> ());
      join (topify_arith_origin e1) (topify_arith_origin e2)


  let arithmetic_function = import_function ~topify_arith_origin

  let unary_arithmetic_function  ~with_alarms info f e1 =
    try
      let v1 = project_ival e1 in
      inject_ival (f v1)
    with Not_based_on_null  ->
      (match with_alarms.imprecision_tracing with
       | Aignore -> ()
       | Acall f -> f ()
       | Alog _ -> match e1 with
         | Map _ ->
           Kernel.warning ~once:true ~current:true
             "Operation %s %a incurs a loss of precision"
             info pretty e1
         | _ -> ());
      topify_arith_origin e1

 let cast_float_to_int ~signed ~size v =
   try
     let v1 = project_ival v in
     let alarm_use_as_float, alarm_overflow, r =
       Ival.cast_float_to_int ~signed ~size v1
     in
     alarm_use_as_float, alarm_overflow, inject_ival r
   with Not_based_on_null ->
     true, true, topify_arith_origin v

 let cast_int_to_float ~with_alarms rounding_mode v =
   unary_arithmetic_function ~with_alarms "integer conversion to float"
     (fun i ->
       let ok, r = Ival.cast_int_to_float rounding_mode i in
       if not ok then
         Kernel.warning ~current:true ~once:true
           "TODO: overflow in integer conversion to float";
       r)
     v

  let div ~with_alarms e1 e2 =
    if equal e2 singleton_one
    then e1
    else begin
      if (with_alarms.others <> Aignore) && contains_zero e2 then CilE.warn_div with_alarms;
      arithmetic_function ~with_alarms "/" Ival.div e1 e2
    end

  let c_rem ~with_alarms e1 e2 =
    if (with_alarms.others <> Aignore) && contains_zero e2 then CilE.warn_div with_alarms;
    arithmetic_function ~with_alarms "%" Ival.c_rem e1 e2

  (** Warn about overflow iff [size] is not [None]. Beware when calling
      this function *)
  let shift_left ~topify_arith_origin ~with_alarms ~size e1 e2 =
    let default () = 
          begin
            try
              let size = Extlib.opt_map Int.of_int size in
              import_function
                ~topify_arith_origin
                ~with_alarms
                "<<"
                (Ival.shift_left ~size) e1 e2
            with Not_found ->
              join (topify_arith_origin e1) (topify_arith_origin e2)
          end
    in
    match size with
      | None -> default ()
      | Some size ->
          let size_int = Int.of_int size in
          let valid_range_rhs =
            inject_ival 
	      (Ival.inject_range 
		  (Some Int.zero)
                  (Some (Int.pred size_int))) 
	  in
	  let valid_range_lhs = 
	    inject_ival 
	      (Ival.inject_range 
		  (Some Int.zero)
		  None)
	  in
          if (with_alarms.others <> Aignore)
	  then begin
              if not (is_included e2 valid_range_rhs)
              then warn_shift with_alarms size;
              if not (is_included e1 valid_range_lhs)
              then warn_shift_left_positive with_alarms;
	    end;
	  if not ((intersects e2 valid_range_rhs) &&
		     (intersects e1 valid_range_lhs))
	  then            bottom
	  else default ()


  let oper_on_values ~with_alarms info f v1 v2 =
    arithmetic_function with_alarms info (Ival.apply_set f) v1 v2


  let shift_right ~with_alarms ~size e1 e2 =
    let default () = 
          begin
            try
              let size = Extlib.opt_map Int.of_int size in
              arithmetic_function ~with_alarms ">>"
                (Ival.shift_right ~size) e1 e2
            with Not_found ->
              join (topify_arith_origin e1) (topify_arith_origin e2)
          end
    in
    match size with
      | None -> default ()
      | Some size ->
          let size_int = Int.of_int size in
          let valid_range =
            inject_ival (Ival.inject_range (Some Int.zero)
                                           (Some (Int.pred size_int)))
          in
          if not (intersects e2 valid_range) then begin
            warn_shift  with_alarms size;
            if with_alarms.others <> Aignore then
              Kernel.warning ~once:true ~current:true
                "invalid shift of %a-bit value by %a. \
This path is assumed to be dead."
                Int.pretty size_int
                pretty e2;
            bottom
          end else begin
            if (with_alarms.others <> Aignore)
              && not (is_included e2 valid_range)
            then warn_shift with_alarms size;
            default ()
          end

  let bitwise_and ~signed ~size e1 e2 =
    let bitwise_and_pointer_ival p _ival =
      Location_Bytes.location_shift
        (Ival.inject_top None (Some Int.zero) Int.zero Int.one)
        p
    in
      try
        let v1 = project_ival e1 in
        try
          let v2 = project_ival e2 in
          let result = Ival.bitwise_and ~signed ~size v1 v2
          in
          inject_ival result
        with Not_based_on_null ->
          bitwise_and_pointer_ival e2 v1
      with Not_based_on_null  ->
        try
          let v2 = project_ival e2 in
          bitwise_and_pointer_ival e1 v2
        with Not_based_on_null ->
          join (topify_arith_origin e1) (topify_arith_origin e2)

  let bitwise_or ~topify_arith_origin ~size e1 e2 =
    try
      let v1 = project_ival e1 in
      let v2 = project_ival e2 in
      let result = Ival.bitwise_or ~size v1 v2
      in
      inject_ival result
    with Not_based_on_null ->
      join (topify_arith_origin e1) (topify_arith_origin e2)

  let extract_bits ~start ~stop v =
    try
      let i = project_ival v in
      false, inject_ival (Ival.extract_bits ~start ~stop i)
    with
      | Not_based_on_null ->
          if is_imprecise v
          then false, v
          else true, topify_misaligned_read_origin v

  let big_endian_merge_bits ~conflate_bottom ~total_length ~length ~value ~offset acc =
    if is_bottom acc || is_bottom value
    then begin
        if conflate_bottom
        then
          bottom
        else
          join
            (topify_misaligned_read_origin acc)
            (topify_misaligned_read_origin value)
      end
    else
    let total_length_i = Int.of_int total_length in
    assert (Int.le (Int.add length offset) total_length_i);
    let result =
      bitwise_or
        ~topify_arith_origin:topify_misaligned_read_origin
        ~size:total_length
        (shift_left
            ~topify_arith_origin:topify_misaligned_read_origin
            ~with_alarms:warn_none_mode
            ~size:(Some total_length)
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

  let little_endian_merge_bits ~conflate_bottom ~total_length ~value
      ~offset acc =
    if is_bottom acc || is_bottom value
    then begin
        if conflate_bottom
        then
          bottom
        else
          join
            (topify_misaligned_read_origin acc)
            (topify_misaligned_read_origin value)
      end
    else
    let result =
      bitwise_or
        ~topify_arith_origin:topify_misaligned_read_origin
        ~size:total_length
        (shift_left
            ~topify_arith_origin:topify_misaligned_read_origin
           ~with_alarms:warn_none_mode
           ~size:(Some total_length)
           value
           (inject_ival (Ival.inject_singleton offset)))
        acc
    in
    (*Format.printf "le merge_bits : total_length:%d value:%a offset:%a acc:%a GOT:%a@."
      total_length pretty value Int.pretty offset pretty acc pretty result;*)
    result

  let all_values ~size v =
    (Kernel.Overflow.get ()) &&
      try
        let i = project_ival v in
          Ival.all_values ~size i
      with Not_based_on_null -> false

  let anisotropic_cast ~size v =
    if all_values ~size v then top_int else v

  let create_all_values ~modu ~signed ~size =
    inject_ival (Ival.create_all_values ~modu ~signed ~size)

  let bitwise_or = bitwise_or ~topify_arith_origin
  let shift_left = shift_left ~topify_arith_origin

  let has_sign_problems v =
      not (is_included top_int v || is_included v top_float)

end

module V_Or_Uninitialized = struct

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

  let get_flags : tt -> int = fun v -> Obj.tag (Obj.repr v)

  let create : int -> V.t -> tt = fun flags v ->
    match flags with
    | 0 -> C_uninit_esc v
    | 1 -> C_uninit_noesc v
    | 2 -> C_init_esc v
    | 3 -> C_init_noesc v
    | _ -> assert false

  let project x = get_v x

(* let (==>) = (fun x y -> (not x) || y) *)

  type widen_hint = V.widen_hint
  let widen wh t1 t2 =
    create (get_flags t2) (V.widen wh (get_v t1) (get_v t2))

  let equal t1 t2 =
    (get_flags t1) = (get_flags t2) &&
    V.equal (get_v t1) (get_v t2)

  exception Error_Bottom
  exception Error_Top

  let join t1 t2 =
(*
    {
      initialized = t1.initialized && t2.initialized;
      no_escaping_adr = t1.no_escaping_adr && t2.no_escaping_adr;
      v = V.join t1.v t2.v
    }
*)
    create
      ((get_flags t1) land (get_flags t2))
      (V.join (get_v t1) (get_v t2))

  let narrow t1 t2 =
(*    {initialized = t1.initialized || t2.initialized;
     no_escaping_adr = t1.no_escaping_adr || t2.no_escaping_adr;
     v = V.narrow t1.v t2.v
    }
*)
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

  let uninitialized = C_uninit_noesc V.bottom
  let initialized v = C_init_noesc v

  let escaping_addr = C_init_esc V.bottom

  let is_included t1 t2 =
(*    (t2.initialized ==> t1.initialized) &&
    (t2.no_escaping_adr ==> t1.no_escaping_adr) &&
      V.is_included t1.v t2.v
*)
    let flags1 = get_flags t1 in
    let flags2 = get_flags t2 in
    (lnot flags2) lor flags1 = -1 &&
        V.is_included (get_v t1) (get_v t2)

  let is_included_exn t1 t2 =
    if not (is_included t1 t2)
    then raise Abstract_interp.Is_not_included

  let intersects _t1 _t2 =
    assert false
(*
    ((not t2.initialized) && (not t1.initialized)) ||
    ((not t2.no_escaping_adr) && (not t1.no_escaping_adr)) ||
      V.intersects t1.v t2.v
*)

  let pretty fmt t =
    let flags = get_flags t in
    let no_escaping_adr = is_noesc flags in
    let initialized = is_initialized flags in
    let v = get_v t in
    if initialized && no_escaping_adr
    then V.pretty fmt v
    else if equal t uninitialized
    then Format.fprintf fmt "UNINITIALIZED"
    else if equal t escaping_addr
    then Format.fprintf fmt "ESCAPINGADDR"
    else if initialized && not no_escaping_adr
    then Format.fprintf fmt "%a or ESCAPINGADDR" V.pretty v
    else if (not initialized) && no_escaping_adr
    then Format.fprintf fmt "%a or UNINITIALIZED" V.pretty v
    else Format.fprintf fmt "%a or UNINITIALIZED or ESCAPINGADDR" V.pretty v

  let cardinal_zero_or_one t =
    match t with
      C_init_noesc v -> V.cardinal_zero_or_one v
    | C_init_esc v | C_uninit_noesc v -> V.is_bottom v
    | C_uninit_esc _ -> false

  let cardinal_less_than t b =
    match t with
      C_init_noesc v -> V.cardinal_less_than v b
    | _ -> raise Abstract_interp.Not_less_than

  let tag t = (get_flags t) * 4513 + (V.tag (get_v t))

  include Datatype.Make
      (struct
        type t = tt (* =     | C_uninit_esc of V.t
                       | C_uninit_noesc of V.t
                       | C_init_esc of V.t
                       | C_init_noesc of V.t *)
        let name = "Cvalue.V_Or_Uninitialized"
        let structural_descr =
           Structural_descr.Structure
             (Structural_descr.Sum
                [| [| V.packed_descr |];
                   [| V.packed_descr |];
                   [| V.packed_descr |];
                   [| V.packed_descr |] |])
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
        let hash = tag
        let equal = equal
        let compare = Datatype.undefined
        let copy = Datatype.undefined
        let rehash = Datatype.identity
        let pretty = pretty
        let internal_pretty_code = Datatype.undefined
        let varname = Datatype.undefined
        let mem_project = Datatype.never_any_project
       end)

  module Top_Param = V.Top_Param

  let is_isotropic t = V.is_isotropic (get_v t)

  let cast ~with_alarms ~size ~signed t =
    create (get_flags t) (V.cast ~with_alarms ~size ~signed (get_v t))

  let extract_bits ~start ~stop t =
    let inform_extract_pointer_bits, v =
      V.extract_bits ~start ~stop (get_v t)
    in
    inform_extract_pointer_bits,
    create (get_flags t) v

  let little_endian_merge_bits ~conflate_bottom ~total_length ~value ~offset t =
    create
      ((get_flags t) land (get_flags value))
      (V.little_endian_merge_bits ~conflate_bottom
          ~total_length ~value:(get_v value) ~offset
          (get_v t))

  let big_endian_merge_bits ~conflate_bottom ~total_length ~length ~value ~offset t =
    create
      ((get_flags t) land (get_flags value))
      (V.big_endian_merge_bits ~conflate_bottom
          ~total_length ~length
          ~value:(get_v value)
          ~offset
          (get_v t))

  let topify_merge_origin t =
    create
      (get_flags t)
      (V.topify_merge_origin (get_v t))

  let topify_arith_origin t =
    create
      (get_flags t)
      (V.topify_arith_origin (get_v t))

  let topify_misaligned_read_origin t =
    create
      (get_flags t)
      (V.topify_misaligned_read_origin (get_v t))

  let topify_with_origin o t =
    create
      (get_flags t)
      (V.topify_with_origin o (get_v t))

  let anisotropic_cast ~size t =
    create
      (get_flags t)
      (V.anisotropic_cast ~size (get_v t))

  let inject_top_origin o t =
    C_init_noesc (V.inject_top_origin o t)

  let under_topify t =
    create
      (get_flags t)
      (V.under_topify (get_v t))

  let of_char c = C_init_noesc (V.of_char c)
  let of_int64 c = C_init_noesc (V.of_int64 c)

  let singleton_zero = C_init_noesc (V.singleton_zero)

  let unspecify_escaping_locals is_local t =
    let flags = get_flags t in
    let flags = flags land mask_init
      (* clear noesc flag *)
    in
    let locals, v =
      V.remove_escaping_locals is_local (get_v t)
    in
    locals, create flags v

  let change_initialized init v = match init, v with
    | true, C_uninit_esc v -> C_init_esc v
    | true, C_uninit_noesc v -> C_init_noesc v
    | true, _ -> v
    | false, C_init_esc v -> C_uninit_esc v
    | false, C_init_noesc v -> C_uninit_noesc v
    | false, _ -> v

  let pretty_c_assert prampamp lv s fmt v =
    V.pretty_c_assert prampamp lv s fmt (get_v v)

end

module V_Offsetmap = Offsetmap.Make(V_Or_Uninitialized)

(*
module R_V =
struct
  include V
end

module C_Offsetmap = Offsetmap.Make(R_V)
*)

module V_Offsetmap_ext = V_Offsetmap
(*struct
  type y = V_Offsetmap.y
  type widen_hint = V_Offsetmap.widen_hint
  type t = V_Offsetmap.t * V_Offsetmap.t
  type tt = t

  let pretty fmt (o,c) =
    V_Offsetmap.pretty fmt o

  let pretty_typ typ fmt (o,c) =
    V_Offsetmap.pretty_typ typ fmt o

  let pretty_debug _ = assert false

  let equal (o1, c1) (o2, c2) =
    V_Offsetmap.equal o1 o2 && C_Offsetmap.equal c1 c2

  let empty = V_Offsetmap.empty, C_Offsetmap.empty

  let is_empty = equal empty

  module Datatype =
    Project.Datatype.Register
      (struct
        type t = tt
        let rehash = assert false
        let copy _ = assert false (* TODO *)
        let before_load () = ()
        let after_load () = assert false
        let name = Project.Datatype.Name.extend "ext" V_Offsetmap.Datatype.name
        let dependencies = [ V_Offsetmap.Datatype.self ]
      end)
end*)

module Partial_lmap = Lmap.Make_LOffset(V_Or_Uninitialized)(V_Offsetmap_ext)

module Default_offsetmap = struct

  let initialized_var_table = Cil_datatype.Varinfo.Hashtbl.create 17

  let create_initialized_var varinfo validity initinfo =
    Cil_datatype.Varinfo.Hashtbl.add
      initialized_var_table varinfo initinfo;
    Base.create_initialized varinfo validity

  let default_offsetmap base = match base with
  | Base.Initialized_Var (v,_) ->
      (try Cil_datatype.Varinfo.Hashtbl.find initialized_var_table v
        with Not_found ->
          V_Offsetmap.empty)
  | Base.Var _ ->
      begin
        match Base.validity base with
          Base.All ->
            let upb = Bit_utils.max_bit_address () in
            V_Offsetmap.add_internal
              (Int.zero, upb)
              (Int.zero, Int.one, V_Or_Uninitialized.uninitialized)
              V_Offsetmap.empty
        | Base.Known (mn, mx) | Base.Unknown (mn, mx) ->
            if Int.ge mx mn
            then
              V_Offsetmap.add_internal
                (mn, mx)
                (Int.zero, Int.one, V_Or_Uninitialized.uninitialized)
                V_Offsetmap.empty
            else
              V_Offsetmap.empty
        | Base.Periodic (mn, mx, p) ->
            assert (Int.is_zero mn);
            let upb = Int.pred p in
            assert (Int.ge mx upb);
            V_Offsetmap.add_internal
              (Int.zero, upb)
              (Int.zero, Int.one, V_Or_Uninitialized.bottom)
              V_Offsetmap.empty
      end
  | Base.Null -> V_Offsetmap.empty
  | Base.String (_,e) -> V_Offsetmap.from_cstring (Base.get_string e)
end

module Model = struct

  include Partial_lmap.Make(Default_offsetmap)
  type y = V.t

  let join x y = snd (join x y)

  let reduce_equality state _l _r = state

  let pretty_c_assert fmt m =
    Format.fprintf fmt "@[";
    (match m with
      Bottom -> Format.fprintf fmt "0"
    | Map m ->
        let first = ref true in
        let print_ampamp () =
          if !first
          then
            first := false
          else
            Format.fprintf fmt "@\n&& ";
        in
        LBase.iter
          (fun base offs ->
            match base with
              Base.Var(v,_) ->
                let typ = unrollType v.Cil_types.vtype in
                let name = v.Cil_types.vname in
                V_Offsetmap.pretty_c_assert_typ name typ print_ampamp fmt offs
            | _ -> ())
          m
    | Top -> Format.fprintf fmt "1");
    Format.fprintf fmt "@]"

  let find_unspecified = find ~conflate_bottom:false

  let find ~conflate_bottom ~with_alarms x y =
    let v = find ~conflate_bottom ~with_alarms x y in
    let v_v = V_Or_Uninitialized.get_v v in
    let bottom = V.is_bottom v_v in
    let flags = V_Or_Uninitialized.get_flags v in

    (* distasteful FIXME *) if conflate_bottom then begin
        if not (V_Or_Uninitialized.is_initialized flags)
        then warn_uninitialized with_alarms;
        if not (V_Or_Uninitialized.is_noesc flags)
        then warn_escapingaddr with_alarms;
      end;

    if with_alarms.unspecified <> Aignore &&
      bottom &&
      not (V_Or_Uninitialized.is_initialized flags &&
             V_Or_Uninitialized.is_noesc flags )
    then begin
      match with_alarms.unspecified with
      | Aignore -> assert false
      | Acall f -> f ()
      | Alog _ ->
        Kernel.warning ~current:true ~once:true
          "completely undefined value in %a."
          Locations.pretty y;
      end;
    v_v

  let has_been_initialized base state =
    try
      let o = find_base base state in
      V_Offsetmap.is o V_Or_Uninitialized.uninitialized
    with Not_found -> true

  let add_binding_not_initialized acc loc =
    add_binding ~with_alarms:warn_none_mode ~exact:true acc loc (V_Or_Uninitialized.uninitialized)

  let add_binding_unspecified acc loc v =
    add_binding ~with_alarms:warn_none_mode ~exact:true acc loc v

  let add_binding ~with_alarms ~exact acc loc value =
    add_binding ~with_alarms ~exact acc loc (V_Or_Uninitialized.initialized value)

 let reduce_binding ~with_alarms acc loc value =
    reduce_binding  ~with_alarms acc loc (V_Or_Uninitialized.initialized value)

  let create_initial ~base ~v ~modu ~state  =
    create_initial ~base ~v:(V_Or_Uninitialized.initialized v) ~modu ~state

  let uninitialize_locals blocks state =
    List.fold_left
      (fun acc block ->
        List.fold_left
          (fun acc vi ->
	    let base = Base.create_varinfo vi in
	    remove_base base acc)
          acc
          block.Cil_types.blocals)
      state
      blocks

 let clear_state_from_locals fundec state =
    let locals = List.map Base.create_varinfo fundec.Cil_types.slocals in
    let formals = List.map Base.create_varinfo fundec.Cil_types.sformals in
    let cleanup acc v = remove_base v acc in
    let result = List.fold_left cleanup state locals in
    List.fold_left cleanup result formals

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
