(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

  let id = "Location_Bytes"

  let project x = x    

  let is_top v =
    match v with 
      | Top _ -> true 
      | _ -> false

  let is_bottom v = equal v bottom
      
  let is_isotropic v =
    is_zero v 
    || equal top_int v 
    || is_top v 
    || is_bottom v

  let contains_zero v =
    match v with
      | Top _ -> true
      | Map m -> 
	let o = find_or_bottom Base.null m in
	Ival.contains_zero o

  let contains_non_zero v =
    not ((equal v bottom) || (is_zero v))

  let of_char c = inject_ival (Ival.of_int (Char.code c))

  exception Not_based_on_null 

  let find_ival m =
    try 
      let k, v = find_lonely_key m in
      if not (Base.is_null k)
      then raise Not_based_on_null
      else v
    with Not_found -> raise Not_based_on_null

  let filter_comparison ival_filter e1 ~cond_expr =
    match e1 with
      | Top _  -> e1
      | Map m1 ->
	  try
	    let k,v2 = find_lonely_key cond_expr in
	    let v1 = find_or_bottom k m1 in
	      Map (add_or_bottom k (ival_filter v1 v2) m1)
	  with Not_found -> e1

  let filter_comparison_float float_filter e1 ~cond_expr =
    try
      let v1 = find_ival e1 in
      let v2 = find_ival cond_expr in
      inject_ival (float_filter v1 v2)
    with Not_based_on_null -> e1

  let filter_le e1 ~cond_expr = filter_comparison Ival.filter_le e1 ~cond_expr
  let filter_ge e1 ~cond_expr = filter_comparison Ival.filter_ge e1 ~cond_expr
  let filter_lt e1 ~cond_expr = filter_comparison Ival.filter_lt e1 ~cond_expr
  let filter_gt e1 ~cond_expr = filter_comparison Ival.filter_gt e1 ~cond_expr

  let filter_le_float e1 ~cond_expr = 
    filter_comparison_float Ival.filter_le_float e1 ~cond_expr
  let filter_ge_float e1 ~cond_expr = 
    filter_comparison_float Ival.filter_ge_float e1 ~cond_expr
  let filter_lt_float e1 ~cond_expr = 
    filter_comparison_float Ival.filter_lt_float e1 ~cond_expr
  let filter_gt_float e1 ~cond_expr = 
    filter_comparison_float Ival.filter_gt_float e1 ~cond_expr

  let pretty fmt v =
    (*Format.printf "@[HERE@.@]";*)
    let pretty_org fmt org = 
      if not (Origin.is_top org) then 
        Format.fprintf fmt " (origin: %a)" Origin.pretty org
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
	    Ival.pretty fmt (find_ival v)
	  with 
            | Not_based_on_null ->
	        let print_binding k v = 
	          if Ival.equal Ival.singleton_zero v 
	          then Format.fprintf fmt " &%a ;" Base.pretty k 
	          else 
		    Format.fprintf fmt " &%a + %a ;" 
		      Base.pretty k Ival.pretty v
	        in
	          Format.fprintf fmt "{{";
	          (M.iter print_binding) m;
	          Format.fprintf fmt "}}"
      
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
      Location_Bytes.location_shift (find_ival v1) v2
    with Not_based_on_null  ->
      try
        Location_Bytes.location_shift (find_ival v2) v1
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
	let right = Ival.scale_int64base factor (find_ival e2)
	in Location_Bytes.location_shift right e1
      with Not_based_on_null  -> (* from [find_ival] *)
        join (topify_arith_origin e1) (topify_arith_origin e2)

  let rec check_equal positive e1 e2 =
    let one,zero = 
      if positive then Ival.singleton_one, Ival.singleton_zero else
        Ival.singleton_zero, Ival.singleton_one
    in
    let result = inject_ival
      (if cardinal_zero_or_one e1 &&
         cardinal_zero_or_one e2 &&
         (equal e1 e2)
       then one
       else 
         if intersects e1 e2 
	   (* [intersects] gives an exact answer *)
         then Ival.zero_or_one
         else zero)
    in
    (* Format.printf "check_equal: positive:%b e1=%a e2=%a result=%a@\n"
      positive
      pretty e1
      pretty e2
      pretty result
    ;*)
    result

  let compare_min_max min max = 
    match min,max with
    | None,_ -> -1
    | _,None -> -1
    | Some min, Some max -> Int.compare min max

  let compare_max_min max min = 
    match max,min with
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

  let comparisons _info f e1 e2 = 
    try
      let k1,v1 = find_lonely_key e1 in
      let k2,v2 = find_lonely_key e2 in
      if Base.compare k1 k2 <> 0 
      then zero_or_one
      else Ival.compare_C f v1 v2
    with Not_found ->
      zero_or_one

  let cast ~with_alarms ~size ~signed expr = 
    try
      let i = find_ival expr in
	inject_ival (Ival.cast ~size ~signed ~value:i)
    with 
      | Not_based_on_null ->
	  if Int.compare size (Int.of_int (Bit_utils.sizeofpointer ())) >= 0 
	    || (match expr with Top _ -> true | _ -> false)
	  then expr
	  else begin
             if is_bottom expr || is_top expr then expr
             else begin 
               if with_alarms.imprecision_tracing then 
                 CilE.warn_once 
                   "casting address to a type smaller than sizeof(void*): @[%a@]"
                   Location_Bytes.pretty expr ;
	       topify_arith_origin expr
             end
	  end

  let arithmetic_function ~with_alarms info f e1 e2 = 
    try
      let v1 = find_ival e1 in
      let v2 = find_ival e2 in
      inject_ival (f v1 v2)
    with Not_based_on_null  -> 
      if with_alarms.imprecision_tracing then 
        begin match e1,e2 with
        | Map _, Map _ -> 
            CilE.warn_once "unsupported %s on addresses(%a,%a)" info 
              pretty e1
              pretty e2
        | _ -> ()
        end;
      join (topify_arith_origin e1) (topify_arith_origin e2)

  let unary_arithmetic_function  ~with_alarms info f e1 = 
    try
      let v1 = find_ival e1 in
      inject_ival (f v1)
    with Not_based_on_null  -> 
      if with_alarms.imprecision_tracing then 
        begin match e1 with
        | Map _ -> 
            warn_once "unsupported %s on addresses(%a)" info pretty e1
        | _ -> ()
        end;
      topify_arith_origin e1

 let cast_float_to_int ~with_alarms v =
   try
     let v1 = find_ival v in
     let f = Ival.project_float v1 in
     inject_ival (Ival.cast_float_to_int f)
   with Ival.Float_abstract.Nan_or_infinite | Not_based_on_null ->
     if with_alarms.imprecision_tracing 
     then warn_once "cast float to int : alarm (TODO)";
     topify_arith_origin v
     
 let cast_int_to_float ~with_alarms v =
   unary_arithmetic_function ~with_alarms "integer promotion" Ival.cast_int_to_float v

  let div ~with_alarms e1 e2 = 
    if equal e2 singleton_one 
    then e1
    else begin
      if (with_alarms.others <> Aignore) && contains_zero e2 then CilE.warn_div with_alarms;
      arithmetic_function ~with_alarms "/" Ival.div e1 e2
    end
      
  let shift_left ~with_alarms ~size e1 e2 = 
    let size_int = Int.of_int size in
    let valid_range = 
      inject_ival (Ival.inject_range (Some Int.zero) (Some (Int.pred size_int))) 
    in
    if not (intersects e2 valid_range) then begin
      (warn_shift with_alarms size;
       if (with_alarms.others <> Aignore) then warn_once 
         "invalid shift of %a-bit value by %a. This path is assumed to be dead."
	 Int.pretty size_int
         pretty e2);
      bottom
    end else 
      match e2 with 
      | Top _ -> 
          warn_shift with_alarms size; 
          join (topify_arith_origin e1) (topify_arith_origin e2)
      | Map m -> 
          begin
            if (with_alarms.others <> Aignore) 
	      && not (is_included e2 valid_range) 
            then warn_shift with_alarms size;
            try 
              let e2 = inject_ival (M.find Base.null m) in
              arithmetic_function ~with_alarms "<<" (Ival.shift_left ~size:size_int) e1 e2
            with Not_found -> 
              join (topify_arith_origin e1) (topify_arith_origin e2)
          end

  let oper_on_values ~with_alarms info f v1 v2 =
    arithmetic_function with_alarms info (Ival.apply_set info f) v1 v2 

  let shift_right ~with_alarms ~size ~signed:_ e1 e2 = 
    let size_int = Int.of_int size in
    let valid_range = 
      inject_ival (Ival.inject_range (Some Int.zero) (Some (Int.pred size_int))) 
    in
    if not (intersects e2 valid_range) then begin
      (warn_shift  with_alarms size;
       if (with_alarms.others <> Aignore) then 
         warn_once 
           "invalid shift of %a-bit value by %a. This path is assumed to be dead."
	Int.pretty size_int
	   pretty e2);
      bottom
    end else 
      match e2 with 
      | Top _ -> warn_shift with_alarms size; 
          join (topify_arith_origin e1) (topify_arith_origin e2)
      | Map m -> 
          begin
            if (with_alarms.others <> Aignore) 
	      && not (is_included e2 valid_range) 
            then CilE.warn_shift with_alarms size;
            try 
              let e2 = inject_ival (M.find Base.null m) in
              arithmetic_function ~with_alarms ">>" 
                (Ival.shift_right ~size:size_int) e1 e2
            with Not_found ->
              join (topify_arith_origin e1) (topify_arith_origin e2)
          end

  let bitwise_and ~size e1 e2 =
    let bitwise_and_pointer_ival p _ival =
      Location_Bytes.location_shift 
	(Ival.inject_top None (Some Int.zero) Int.zero Int.one)
	p 
    in
      try
	let v1 = find_ival e1 in
	try
	  let v2 = find_ival e2 in
	  let result = Ival.bitwise_and ~size v1 v2	    
	  in
	  inject_ival result
	with Not_based_on_null ->
	  bitwise_and_pointer_ival e2 v1
      with Not_based_on_null  -> 
	try
	  let v2 = find_ival e2 in
	  bitwise_and_pointer_ival e1 v2
	with Not_based_on_null -> 
	  join (topify_arith_origin e1) (topify_arith_origin e2)

  let bitwise_or ~size e1 e2 =
    try
      let v1 = find_ival e1 in
      let v2 = find_ival e2 in
      let result = Ival.bitwise_or ~size v1 v2	    
      in
      inject_ival result
    with Not_based_on_null ->
      join (topify_arith_origin e1) (topify_arith_origin e2)

  let extract_bits ~start ~stop v = 
    try
      let i = find_ival v in
	inject_ival (Ival.extract_bits ~start ~stop i)
    with 
      | Not_based_on_null -> 
          if is_top v then v
          else begin 
            CilE.warn_once "extracting bits of a pointer";
            topify_arith_origin v
          end

  let big_endian_merge_bits ~total_length ~length ~value ~offset acc =
    let total_length_i = Int.of_int total_length in
    assert (Int.le (Int.add length offset) total_length_i);
    let result = 
      bitwise_or
	~size:total_length
	(shift_left 
	    ~with_alarms:warn_none_mode
	    ~size:total_length
	    value
	   (inject_ival (Ival.inject_singleton (Int.sub (Int.sub total_length_i offset) length))))
	acc
    in
(*    Format.printf "be merge_bits : total_length:%d length:%a value:%a offset:%a acc:%a GOT:%a@." 
      total_length 
      Int.pretty length 
      pretty value 
      Int.pretty offset
      pretty acc
      pretty result; *)
    result

  let little_endian_merge_bits ~total_length ~value ~offset acc =
    let result = 
      bitwise_or
        ~size:total_length
        (shift_left 
           ~with_alarms:warn_none_mode
           ~size:total_length
           value
           (inject_ival (Ival.inject_singleton offset)))
        acc
    in
    (*Format.printf "le merge_bits : total_length:%d value:%a offset:%a acc:%a GOT:%a@." 
      total_length pretty value Int.pretty offset pretty acc pretty result;*)
    result
      
  let all_values ~size v = 
    (not (Cmdline.IgnoreOverflow.get ())) &&
      try 
        let i = find_ival v in
          Ival.all_values ~size i
      with Not_based_on_null -> false
        
  let anisotropic_cast ~size v = 
    if all_values ~size v then topify_arith_origin v else v

end

let (==>) = (fun x y -> (not x) || y)

module V_Or_Uninitialized = struct
  type t = { initialized : bool;
             v : V.t}
  let id = "V_Or_Uninitialized"
  let project x = V.project x.v

  let is_included_actual_generic b1 b2 instanciation t1 t2 =
    if t2.initialized ==> t1.initialized then 
      V.is_included_actual_generic b1 b2 instanciation t1.v t2.v
    else raise Abstract_interp.Is_not_included

  type widen_hint = V.widen_hint
  let widen wh t1 t2 = { initialized = t2.initialized; 
                         v = V.widen wh t1.v t2.v}
  let equal t1 t2 = 
    t1.initialized=t2.initialized && V.equal t1.v t2.v
  exception Error_Bottom
  exception Error_Top
  let join t1 t2 = 
    {initialized = t1.initialized && t2.initialized;
     v = V.join t1.v t2.v
    }
  let narrow t1 t2 = 
    {initialized = t1.initialized && t2.initialized;
     v = V.narrow t1.v t2.v
    }
  let link t1 t2 = 
    {initialized = t1.initialized || t2.initialized;
     v = V.link t1.v t2.v
    }
  let meet t1 t2 = 
    {initialized = t1.initialized || t2.initialized;
     v = V.meet t1.v t2.v
    }
      
  let bottom = { initialized = true;
                 v = V.bottom;}
  let top =  { initialized = false;
               v = V.top;}
  let uninitialized = { initialized = false;
                        v = V.bottom;}

  let initialized v = { initialized = true ;
                        v = v;}

  let is_included t1 t2 = 
    (t2.initialized ==> t1.initialized) && V.is_included t1.v t2.v

  let is_included_exn t1 t2 = 
    if t2.initialized ==> t1.initialized then 
      V.is_included_exn t1.v t2.v
    else raise Abstract_interp.Is_not_included

  let intersects t1 t2 = 
    ((not t2.initialized) && (not t1.initialized)) || V.intersects t1.v t2.v

  let pretty fmt t = 
    if t.initialized then V.pretty fmt t.v
    else if equal t uninitialized then Format.fprintf fmt "UNSPECIFIED"
    else Format.fprintf fmt "%a or UNSPECIFIED" V.pretty t.v
      
  let cardinal_zero_or_one t =
    t.initialized && V.cardinal_zero_or_one t.v

  let cardinal_less_than t b = 
    if t.initialized then V.cardinal_less_than t.v b
    else raise Abstract_interp.Not_less_than

  let tag t = 
    if t.initialized then V.tag t.v
    else 17 * (V.tag t.v)

  let hash = tag

  type tt = t
  module Datatype =
    Project.Datatype.Register
      (struct
	 type t = tt
	 let copy _ = assert false (* TODO *)
	 let rehash t = {initialized=t.initialized;
                         v = V.Datatype.rehash t.v}
	 let after_load () = ()
	 let before_load () = ()
	 let name = Project.Datatype.Name.make id
	 let dependencies = [ V.Datatype.self ]
       end)
  module Top_Param = V.Top_Param

  let is_isotropic t = V.is_isotropic t.v

  let cast ~with_alarms ~size ~signed t = 
    {initialized = t.initialized;
     v = V.cast ~with_alarms ~size ~signed t.v}
  let extract_bits ~start ~stop t = 
    {initialized = t.initialized;
     v = V.extract_bits ~start ~stop t.v}

  let bitwise_or ~size t1 t2 = 
    { initialized = t1.initialized && t2.initialized ;
      v = V.bitwise_or ~size t1.v t2.v}

  let shift_left ~with_alarms ~size t1 t2 = 
    {initialized = t1.initialized && t2.initialized ;
     v = V.shift_left ~with_alarms ~size t1.v t2.v}

  let little_endian_merge_bits ~total_length ~value ~offset t =
    {initialized = t.initialized && value.initialized ;
     v = V.little_endian_merge_bits ~total_length ~value:value.v ~offset t.v}

  let big_endian_merge_bits ~total_length ~length ~value ~offset t =
    {initialized = t.initialized && value.initialized;
     v = V.big_endian_merge_bits ~total_length ~length ~value:value.v ~offset t.v}

  let topify_merge_origin t = 
    {initialized = t.initialized;
     v = V.topify_merge_origin t.v}

  let topify_arith_origin t = 
    {initialized = t.initialized;
     v = V.topify_arith_origin t.v}

  let topify_misaligned_read_origin t = 
    {initialized = t.initialized;
     v = V.topify_misaligned_read_origin t.v}

  let topify_with_origin o t = 
    {initialized = t.initialized;
     v = V.topify_with_origin o t.v}

  let anisotropic_cast ~size t = 
    {initialized = t.initialized;
     v = V.anisotropic_cast ~size t.v}

  let inject_top_origin o t = 
    {initialized = true;
     v = V.inject_top_origin o t}

  let under_topify t = 
    {initialized = t.initialized;
     v = V.under_topify t.v}

  let of_char c = 
    {initialized = true;
     v = V.of_char c;}

  let singleton_zero =
    {initialized = true;
     v = V.singleton_zero;}

  let unspecify_escaping_locals fundec t = 
    {initialized = false;
     v = V.unspecify_escaping_locals fundec t.v}
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

module Default_offsetmap = 
struct  
  let initialized_var_table = Cilutil.VarinfoHashtbl.create 17
    
  let create_initialized_var varinfo validity initinfo =
    Cilutil.VarinfoHashtbl.add initialized_var_table varinfo initinfo;
    Base.create_initialized varinfo validity

  let default_offsetmap varid = match varid with
  | Base.Initialized_Var (v,_) ->
      (try Cilutil.VarinfoHashtbl.find initialized_var_table v
        with Not_found ->
          V_Offsetmap.empty)
  | Base.Var _ | Base.Null 
  | Base.Cell_class _ -> V_Offsetmap.empty
  | Base.String (_,s) -> V_Offsetmap.from_string s
end

module Model =
struct
  include Partial_lmap.Make(Default_offsetmap)
  type y = V.t

  let find_unspecified = find

  let find ~with_alarms x y = 
    let v = (find ~with_alarms x y) in
    if not v.V_Or_Uninitialized.initialized then 
      begin warn_unspecified with_alarms;
        if with_alarms.unspecified <> Aignore && V.is_bottom  v.V_Or_Uninitialized.v then
          warn_once 
            "completely unspecified value in %a. This path is assumed to be dead." 
            Locations.pretty y;
      end;
    v.V_Or_Uninitialized.v

  let add_binding_unspecified acc loc =
    add_binding ~with_alarms:warn_none_mode ~exact:true acc loc (V_Or_Uninitialized.uninitialized)

  let add_binding ~with_alarms ~exact acc loc value =
    add_binding ~with_alarms ~exact acc loc (V_Or_Uninitialized.initialized value) 

  let create_initial ~base ~size ~v ~modu ~state  = 
    create_initial ~base ~size ~v:(V_Or_Uninitialized.initialized v) ~modu ~state
end
