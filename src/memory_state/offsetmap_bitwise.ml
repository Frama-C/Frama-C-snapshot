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
open Lattice_Interval_Set

type itv = Int.t * Int.t

module Make(V: Lattice_type.Bounded_Join_Semi_Lattice) =
struct

  module V_bool = struct
    include Datatype.Pair_with_collections(Datatype.Bool)(V)
      (struct let module_name =
                Format.sprintf "Offsetmap_bitwise(%s).Make.V_bool" V.name end)
    let hash (b,v) =
      let h = V.hash v in
      if b then h else 100000 + h

    let fast_equal (b1, v1: t) (b2, v2: t) = b1 = b2 && v1 == v2
  end

  module M = Int_Interv_Map.Make(V_bool)

  type tt = Map of M.t | Degenerate of V.t

  let hash x = match x with
    | Degenerate v -> 571 + V.hash v
    | Map map -> M.hash map

  let empty = Map M.empty

  let degenerate v = Degenerate v

  let equal_map mm1 mm2 =
    try M.equal mm1 mm2
    with Int_Interv.Cannot_compare_intervals -> false

  let equal m1 m2 =
    match m1, m2 with
      Degenerate v1, Degenerate v2 ->
        V.equal v1 v2
    | Map mm1, Map mm2 ->
        equal_map mm1 mm2
    | Map _, Degenerate _ | Degenerate _, Map _ -> false

  let compare =
    if V.compare == Datatype.undefined || M.compare == Datatype.undefined then (
      Kernel.debug "Missing comparison function for %s offsetmap_bitwise \
          (%b, %b)"V.name
        (V.compare == Datatype.undefined) (M.compare == Datatype.undefined);
      Datatype.undefined)
    else
      fun m1 m2 ->
        if m1 == m2 then 0
        else match m1, m2 with
          | Map m1, Map m2 -> M.compare m1 m2
          | Degenerate v1, Degenerate v2 -> V.compare v1 v2
          | Map _, Degenerate _ -> -1
          | Degenerate _, Map _ -> 1


  module MapIntervals =
    FCMap.Make(struct
      type t = Int_Intervals.t
      let compare = Int_Intervals.compare_itvs
    end)

  (* Print a map by fusing together intervals that map to the same value *)
  let fold_fuse_same_aux f m acc =
    let h = V_bool.Hashtbl.create 17 in
    (* Map the various values in m to the intervals they appear in*)
    let sort_by_content itv v () =
      let cur = try V_bool.Hashtbl.find h v
        with Not_found -> Int_Intervals.bottom
      in
      let itvs = Int_Intervals.inject [itv] in
      let new_ = Int_Intervals.join itvs cur in
      V_bool.Hashtbl.replace h v new_
    in
    M.fold sort_by_content m ();
    (* Now sort the contents of h by increasing intervals *)
    let m = V_bool.Hashtbl.fold
      (fun v itvs acc -> MapIntervals.add itvs v acc)
      h MapIntervals.empty
    in
    (* Call f on those intervals *)
    MapIntervals.fold (fun itvs v acc -> f itvs v acc) m acc

  let fold_fuse_same f offsm acc =
    match offsm with
      | Degenerate v ->
          f Int_Intervals.top (true,v) acc
      | Map offsm -> fold_fuse_same_aux f offsm acc

  let range_covers_whole_type typ itvs =
    match typ with
      | None -> false
      | Some typ ->
          match Int_Intervals.project_singleton itvs with
            | Some (b, e) ->
                (try
                   let s = Cil.bitsSizeOf typ in
                   Int.equal b Int.zero && Int.equal e (Int.of_int (pred s))
                 with Cil.SizeOfError _ -> false)
            | None -> false

  let pretty_with_type_generic_printer typ printer sep fmt m =
    match m with
    | Degenerate v ->
        Format.fprintf fmt "@[[..] %s @[%a@]@]" sep printer v
    | Map m ->
        let pp_itv = Int_Intervals.pretty_typ typ in
        let first = ref true in
        let pretty_binding fmt itvs (default,v) () =
          if !first then first := false else Format.fprintf fmt "@," ;
          Format.fprintf fmt "@[<hv>@[%a@]%(%)@[%s @[%a%s@]@]@]"
            pp_itv itvs
            (if range_covers_whole_type typ itvs
             then (" ": (unit,Format.formatter,unit) format) else "@ ")
            sep printer v
            (if default then " (and SELF)" else "")

        in
        Format.fprintf fmt "@[<v>";
        fold_fuse_same_aux (pretty_binding fmt) m ();
        Format.fprintf fmt "@]"

  let pretty_with_type typ fmt m =
    pretty_with_type_generic_printer typ V.pretty "FROM" fmt m

  let pretty = pretty_with_type None

  include Datatype.Make
  (struct
    type t = tt
    let name = V.name ^ " offsetmap_bitwise"
    let structural_descr =
      Structural_descr.t_sum [| [| M.packed_descr |]; [| V.packed_descr |] |]
    let reprs =
      List.fold_left
        (fun acc m -> Map m :: acc)
        (List.map (fun v -> Degenerate v) V.reprs)
        M.reprs
    let equal = equal
    let hash = hash
    let compare = compare
    let pretty = pretty
    let internal_pretty_code = Datatype.undefined
    let rehash = Datatype.identity
    let copy = Datatype.undefined
    let varname = Datatype.undefined
    let mem_project = Datatype.never_any_project
   end)
  let () = Type.set_ml_name ty None

  let is_empty m =
    match m with
      Map m -> M.is_empty m
    | Degenerate _ -> false

  let find default ((bi,ei) as i) m =
    match m with
      Degenerate v -> v
    | Map m ->
        let concerned_intervals =
          M.concerned_intervals Int_Interv.fuzzy_order i m
        in
        match concerned_intervals with
        | [] -> default bi ei
        | ((_bk,ek),_)::_ ->
            (* Beware that intervals are presented in reverse order.
               [(bk, ek)] is the highest one. *)
            let implicit_right =
              if Int.gt ei ek
              then default (Int.succ ek) ei
              else V.bottom
            in
            let rec implicit_mid_and_left list acc =
              match list with
              | [(bl,_el),_] -> (* Implicit left *)
                  if Int.lt bi bl
                  then V.join acc (default bi (Int.pred bl))
                  else acc
              | ((bl, _el), _)::((((_bk, ek), _)::_) as tail) ->
                let tail = implicit_mid_and_left tail acc in
                (* implicit mid, ie. a hole between the two intervals. Again,
                   [(bk, ek)] is the highest interval *)
                (*   Format.printf "treat_mid_itv: ek:%a bl:%a@\n" Int.pretty ek
                     Int.pretty bl; *)
                let s_ek = Int.succ ek in
                if Int.lt s_ek bl then
                  V.join (default s_ek (Int.pred bl)) tail
                else tail
              | [] -> assert false
            in
            let implicit =
              implicit_mid_and_left concerned_intervals implicit_right
            in
            (* now add the explicit values *)
            List.fold_left
              (function acc -> function ((bi',ei'),(d,v)) ->
                 let valu = V.join v acc in
                 if d then
                   let (b, e) = Int_Interv.clip_itv (bi, ei) (bi', ei') in
                   V.join valu (default b e)
                 else valu
              )
              implicit
              concerned_intervals

  let same_values ((bx:bool),x) (by,y) =
    (bx = by) && (V.equal x y )

  let add_map_internal i v map = (* FIXME (?) Fails to stick the writing binding
                                    with neighbors if applicable *)
    match M.cleanup_overwritten_bindings same_values i v map
    with
    | None -> map
    | Some(new_bi, new_ei, cleaned_m) ->
        (* Add the new binding *)
        let result = M.add (new_bi,new_ei) v cleaned_m in
        result

  let merge_map m1 m2 =
    M.fold (fun k v acc -> add_map_internal k v acc) m1 m2

(* low-level add to manipulate the pairs (default,value) *)
  let add_internal ((_bi,_ei) as i) (_bv, tv as v) m =
    match m with
    | Degenerate v1 -> Degenerate (V.join tv v1)
    | Map map ->
        Map (add_map_internal i v map)
(** exact add *)
  let add i v m = add_internal i (false,v) m

(** approximate add, for when the target location is ambiguous *)
  let add_approximate (b, e as i) v m =
   match m with
    | Degenerate v1 -> Degenerate (V.join v v1)
    | Map map ->
        let concerned_intervals =
          M.concerned_intervals Int_Interv.fuzzy_order i map
        in
        let treat_interval (acc, right_bound) ((b1, e1), (d1, v1)) =
          let acc, restricted_e1 =
            if Int.lt e1 right_bound
            then begin (* there is a hole *)
                let i_hole = (Int.succ e1, right_bound) in
                add_internal i_hole (true, v) acc, e1
              end
            else acc, Int.min e1 e
          in
          let restricted_b1 = Int.max b1 b in
          let restricted_i1 = restricted_b1, restricted_e1 in
          add_internal restricted_i1 (d1,V.join v1 v) acc, Int.pred restricted_b1
        in
        let acc, right_bound = List.fold_left treat_interval (m, e) concerned_intervals
        in
        let result =
          if Int.le b right_bound
            then begin (* there is a hole *)
                let i_hole = (b, right_bound) in
                add_internal i_hole (true, v) acc
              end
            else acc
        in
(*      Format.printf "bitwise add_approximate@\ninterval:%a..%a value:%a@\nstate%a@\nresult: %a@."
          Int.pretty b Int.pretty e
          V.pretty v
          pretty m
          pretty result;*)
        result

  let collapse m =
    match m with
    | Degenerate v -> v
    | Map map ->
        M.fold (fun _ (_,v) acc -> V.join acc v) map V.bottom

  let find_iset default alldefault is m =
    let result =
      if Int_Intervals.is_top is
      then
        V.join alldefault (collapse m)
      else
        let s = Int_Intervals.project_set is in
        if s = []
        then V.bottom
        else begin
          match m with
            | Degenerate v ->
                List.fold_left
                  (fun acc i -> V.join acc (default (fst i) (snd i)))
                  v s
            | Map _ ->
                let f acc i =  V.join acc (find default i m) in
                List.fold_left f V.bottom s
        end
    in
(*    Format.printf "find_iset %a %a@\nresult:%a@." Int_Intervals.pretty is pretty m V.pretty result; *)
    result

  let add_iset ~exact is v m =
    if Int_Intervals.is_top is
    then begin
(*      Format.printf "add_iset degenerate: value: %a@\nmap: %a@." V.pretty v pretty m; *)
      Degenerate (V.join v (collapse m))
    end
    else begin
        let s = Int_Intervals.project_set is in
        match m with
        | Degenerate v1 -> Degenerate (V.join v v1)
        | Map _ ->
            let result =
              List.fold_left
                (fun acc i ->
                   (if exact then add else add_approximate)
                   i v acc)
                m
                s
            in
            result
    end

  let joindefault_internal =
    M.map
      (fun v -> true, (snd v))

  let fold f m acc =
    match m with
      | Degenerate v ->
          f Int_Intervals.top (true,v) acc
      | Map m ->
          M.fold
            (fun i v acc ->
               f (Int_Intervals.inject [i]) v acc)
            m
            acc

  let map_map f m =
    M.fold
      (fun i v acc -> add_map_internal i (f v) acc)
      (* [pc] add_internal could be replaced by a more efficient
         function that assumes there are no bindings above i *)
      m
      M.empty

  let map f m =
    match m with
      | Degenerate v -> Degenerate (snd (f (true,v)))
      | Map m ->
          Map (map_map f m)

(*  let check_contiguity m =
    let id = map (fun x -> x) m in
    assert (equal id m)

  let check_map_contiguity m =
    let id = map_map (fun x -> x) m in
    assert (equal_map id m)
*)
  let joindefault m =
    match m with
      Degenerate _ -> m
    | Map m ->
        Map (joindefault_internal m)

  let map2
      (f : (bool * V.t) option -> (bool * V.t) option -> bool * V.t)
      mm1 mm2 =
(*    check_contiguity(mm2);
    check_contiguity(mm1); *)
    let result =
    match mm1, mm2 with
    | Degenerate(v), m | m, Degenerate(v) ->
        Degenerate (snd (f (Some (true, v)) (Some (true, collapse m))))
    | Map(m1), Map(m2) ->
        (*Format.printf "map2: m1:@\n%a@\nm2:@\n%a@\n"
          pretty mm1 pretty mm2;*)
        let compute_remains_m1_and_merge m1 acc =
          let remains =
            map_map
              (fun vv -> f (Some vv) None)
              m1
          in
           merge_map remains acc
        in
        let compute_remains_m2_and_merge m2 acc =
(*        check_map_contiguity(acc); *)
          let remains = map_map
            (fun vv -> f None (Some vv))
            m2
          in
(*        check_map_contiguity(remains); *)
          let result = merge_map remains acc in
(*        check_map_contiguity(result);*)
          result
        in
        let rec out_out (b1,_e1 as i1) v1 m1 (b2, _e2 as i2) v2 m2 acc =
          (*Format.printf "out_out: b1=%a e1=%a b2=%a e2=%a@\n"
            Int.pretty b1 Int.pretty e1 Int.pretty b2 Int.pretty e2; *)
(*        check_map_contiguity(acc);*)
          let result =
            if Int.lt b1 b2
            then in_out i1 v1 m1  i2 v2 m2  acc
            else if Int.gt b1 b2
            then out_in i1 v1 m1  i2 v2 m2  acc
            else (* b1 = b2 *)
              in_in i1 v1 m1  i2 v2 m2  acc
          in
(*        check_map_contiguity(result);*)
          result
        and in_out (b1,e1 as i1) v1 m1  (b2, _e2 as i2) v2 m2  acc =
           (*Format.printf "in_out: b1=%a e1=%a b2=%a e2=%a@\n"
            Int.pretty b1 Int.pretty e1 Int.pretty  b2 Int.pretty e2; *)
(*        check_map_contiguity(acc);*)
          assert (Int.gt b2 b1);
          let result =
            let pb2 = Int.pred b2 in
            let new_v = f (Some v1) None in
            if Int.lt pb2 e1
            then begin (* -> in_in *)
                let new_acc = add_map_internal (b1,pb2) new_v acc in
                in_in (b2,e1) v1 m1  i2 v2 m2  new_acc
              end
            else begin
                let new_acc = add_map_internal i1 new_v acc in
                try
                  let (new_i1, new_v1) = M.lowest_binding m1 in
                  let new_m1 = M.remove new_i1 m1 in
                  if Int.lt e1 pb2
                  then (* -> out_out *)
                    out_out new_i1 new_v1 new_m1 i2 v2 m2  new_acc
                  else (* pb2 = e1 *)
                    (* -> in_or_out_in *)
                    in_or_out_in new_i1 new_v1 new_m1  i2 v2 m2  new_acc
                with M.Empty_rangemap ->
                  compute_remains_m2_and_merge (add_map_internal i2 v2 m2) new_acc
              end
          in
(*        check_map_contiguity(result);*)
          result
        and out_in (b1,_e1 as i1) v1 m1  (b2, e2 as i2) v2 m2  acc =
           (* Format.printf "out_in: b1=%a e1=%a b2=%a e2=%a@\n"
            Int.pretty b1
             Int.pretty e1
             Int.pretty b2
             Int.pretty e2; *)
(*        check_map_contiguity(acc);*)
          assert (Int.lt b2 b1);
          let result =
          let pb1 = Int.pred b1 in
          let new_v = f None (Some v2) in
          if Int.lt pb1 e2
          then begin (* -> in_in *)
            let new_acc = add_map_internal (b2,pb1) new_v acc in
            in_in i1 v1 m1  (b1,e2) v2 m2  new_acc
          end
          else begin
            let new_acc = add_map_internal i2 new_v acc in
            try
              let (new_i2, new_v2) = M.lowest_binding m2 in
              let new_m2 = M.remove new_i2 m2 in
              if Int.lt e2 pb1
              then (* -> out_out *)
                out_out i1 v1 m1 new_i2 new_v2 new_m2  new_acc
              else (* pb1 = e2 *)
                (* -> in_in_or_out *)
                in_in_or_out i1 v1 m1 new_i2 new_v2 new_m2  new_acc
            with M.Empty_rangemap ->
              compute_remains_m1_and_merge (add_map_internal i1 v1 m1) new_acc
            end
          in
(*        check_map_contiguity(result);*)
          result
        and in_in_or_out (b1,_e1 as i1) v1 m1  (b2,_e2 as i2) v2 m2 acc =
            (*Format.printf "in_in_or_out: b1=%a e1=%a b2=%a e2=%a@\n"
             Int.pretty b1 Int.pretty e1 Int.pretty b2 Int.pretty e2;*)
          (if Int.equal b1 b2 then in_in else (assert (Int.lt b1 b2);in_out))
            i1 v1 m1 i2 v2 m2 acc
        and in_or_out_in (b1,_e1 as i1) v1 m1  (b2,_e2 as i2) v2 m2 acc =
           (*Format.printf "in_or_out_in: b1=%a e1=%a b2=%a e2=%a@\n"
            Int.pretty b1
             Int.pretty e1
             Int.pretty b2
             Int.pretty e2;*)
          (if Int.equal b1 b2 then in_in else (assert (Int.gt b1 b2);out_in))
            i1 v1 m1 i2 v2 m2 acc
        and in_in_e1_first (_b1, e1 as i1) _v1 m1 (_b2, e2) v2 m2 acc new_v12 =
          (*Format.printf "in_in_e1_first: b1=%a e1=%a b2=%a e2=%a@\n"
            Int.pretty b1 Int.pretty e1 Int.pretty b2 Int.pretty e2; *)
          assert (Int.lt e1 e2);
          let new_acc = add_map_internal i1 new_v12 acc in
          let new_i2 = (Int.succ e1,e2) in
          try
            let (new_i1, new_v1) = M.lowest_binding m1 in
            let new_m1 = M.remove new_i1 m1 in
            in_or_out_in new_i1 new_v1 new_m1  new_i2 v2 m2  new_acc
          with M.Empty_rangemap ->
            compute_remains_m2_and_merge
              (add_map_internal new_i2 v2 m2) new_acc
        and in_in_e2_first (_b1, e1) v1 m1 (_b2, e2 as i2) _v2 m2 acc new_v12=
          (*Format.printf "in_in_e2_first: b1=%a e1=%a b2=%a e2=%a@\n"
            Int.pretty b1 Int.pretty e1 Int.pretty b2 Int.pretty e2; *)
          assert (Int.lt e2 e1);
          let new_acc = add_map_internal i2 new_v12 acc in
          let new_i1 = (Int.succ e2,e1) in
          try
            let (new_i2, new_v2) = M.lowest_binding m2 in
            let new_m2 = M.remove new_i2 m2 in
            in_in_or_out new_i1 v1 m1  new_i2 new_v2 new_m2  new_acc
          with M.Empty_rangemap ->
            compute_remains_m1_and_merge
              (add_map_internal new_i1 v1 m1) new_acc
        and in_in_same_end (_b1, e1 as i1) _v1 m1 (_b2, e2) _v2 m2 acc new_v12=
          (*Format.printf "in_in_same_end: b1=%a e1=%a b2=%a e2=%a@\n"
            Int.pretty b1 Int.pretty e1 Int.pretty b2 Int.pretty e2; *)
          assert (Int.equal e1 e2);

          let acc = add_map_internal i1 new_v12 acc in
          try
            let (new_i1, new_v1) = M.lowest_binding m1 in
            let new_m1 = M.remove new_i1 m1 in
            try
              let (new_i2, new_v2) = M.lowest_binding m2 in
              let new_m2 = M.remove new_i2 m2 in
              out_out new_i1 new_v1 new_m1  new_i2 new_v2 new_m2 acc
            with M.Empty_rangemap ->
              compute_remains_m1_and_merge m1 acc
          with M.Empty_rangemap ->
            compute_remains_m2_and_merge m2 acc
        and in_in (b1, e1 as i1) v1 m1 (b2, e2 as i2) v2 m2 acc =
          (*Format.printf "in_in: b1=%a e1=%a b2=%a e2=%a@\n"
            Int.pretty b1 Int.pretty e1 Int.pretty b2 Int.pretty e2; *)
          assert (Int.equal b1 b2);

          let new_v12 = f (Some v1) (Some v2) in
          (if Int.gt e1 e2
           then in_in_e2_first
           else if Int.lt e1 e2
           then in_in_e1_first
           else in_in_same_end)
            i1 v1 m1  i2 v2 m2  acc  new_v12
        in
        try
          let i1, v1 = M.lowest_binding m1 in
          try
            let i2, v2 = M.lowest_binding m2 in
            let new_m1 = M.remove i1 m1 in
            let new_m2 = M.remove i2 m2 in
            Map (out_out i1 v1 new_m1 i2 v2 new_m2 M.empty)
          with M.Empty_rangemap -> mm1
        with M.Empty_rangemap -> mm2
    in
(*    check_contiguity(result);*)
    result

  let check_inter offs1 offs2 =
    let check bi ei =
      let concerned_intervals =
        M.concerned_intervals
          Int_Interv.fuzzy_order (bi,ei) offs2
      in
      List.iter
        (fun (_,(b,_v)) -> if not b then raise Int_Interv.Not_fully_included)
        concerned_intervals
    in
    let f (bi,ei) _ acc =
      match acc with
        None ->
          (* (* now we do something about -**..bi *)
          if Int.neq bi Int.min_int
          then check Int.min_int (Int.pred bi);*)
          Some ei
      | Some ek ->
          let pbi = Int.pred bi in
          if Int.lt ek pbi
          then check (Int.succ ek) pbi;
          Some ei
    in
    match M.fold f offs1 None with
    | None -> ()
    | Some _ek ->
        (* if Int.lt ek Int.max_int
        then check (Int.succ ek) Int.max_int *)
        ()

  let is_included_aux offs1 offs2 =
    if offs1 != offs2 then
      match offs1, offs2 with
      | Map offs1, Map offs2 ->
          let treat_itv (_bi, _ei as i) (di,vi) =
            let concerned_intervals =
              M.concerned_intervals Int_Interv.fuzzy_order i offs2
            in
            Int_Interv.check_coverage i concerned_intervals;
            List.iter
              (fun ((_bj, _ej),(dj,vj)) ->
                 if (di && (not dj)) || not (V.is_included vi vj) then
                   raise Int_Interv.Not_fully_included;
              ) concerned_intervals
          in
          M.iter treat_itv offs1    ;
          check_inter offs1 offs2
      | Degenerate _v1, Map _offs2 ->
          raise Int_Interv.Not_fully_included
      | _, Degenerate v2 ->
          if not (V.is_included (collapse offs1) v2)
          then raise Int_Interv.Not_fully_included

  let is_included m1 m2 =
    try is_included_aux m1 m2; true
    with Int_Interv.Not_fully_included -> false

  let join mm1 mm2 =
(*    check_contiguity(mm1);
    check_contiguity(mm2);
*)
    if mm1 == mm2 then mm1 else
      let result = map2
        (fun v1 v2 -> match v1,v2 with
           | None, None -> assert false
           | Some v , None | None, Some v -> true, snd v
           | Some v1, Some v2 ->
               (fst v1 || fst v2), (V.join (snd v1) (snd v2)))
        mm1 mm2
      in
(*      check_contiguity(result);*)
      result

  (* map [f] on [offs] and merge with [acc] *)
  let map_and_merge f offs acc =
(*    check_contiguity(acc);
    check_contiguity(offs);*)
    let generic_f v1 v2 = match v1,v2 with
    | None, None -> assert false
    | Some (d,v), None  ->
        d,f v
    | None, Some vv -> vv
    | Some (d1,v1), Some (d2,v2) ->
        d1&&d2,
        if d1 then V.join (f v1) v2 else f v1
    in
(*    Format.printf "@[Offsetmap.map_and_merge offs:%a and acc:%a@]@."
         (pretty) offs
         (pretty) acc;
*)
    let result = map2 generic_f offs acc in
(*    check_contiguity(result);*)
    result

(* this code was copied from the non-bitwise lattice, it could be shared
   if it was placed in M. TODO PC 2007/02 *)
  let copy_paste_map ~f from start stop start_to _to =
    let result =
      let ss = start,stop in
      let to_ss = start_to, Int.sub (Int.add stop start_to) start in
        (* First removing the bindings of the destination interval *)
      let _to = M.remove_itv Int_Interv.fuzzy_order to_ss _to in
      let concerned_itv =
        M.concerned_intervals Int_Interv.fuzzy_order ss from
      in
      let offset = Int.sub start_to start in
      let current = ref start in
      let f, treat_empty_space =
        match f with
          Some (f, default) -> f,
            (fun acc i ->
              let src_b = !current in
              if Int.le i src_b
              then acc
              else
                let src_e = Int.pred i in
                let dest_itv = Int.add (!current) offset, Int.add src_e offset in
                (*          Format.printf "treat_empty ib=%a ie=%a@."
                            Int.pretty src_b
                            Int.pretty src_e;*)
                add_map_internal dest_itv (f (true, default src_b src_e)) acc)
        | None -> (fun x -> x), (fun acc _i -> acc)
      in
      let treat_interval ((b,_) as i,v) acc =
        let acc = treat_empty_space acc b in
        let new_vv = f v in
        let src_b, src_e = Int_Interv.clip_itv ss i in
        let dest_i = Int.add src_b offset, Int.add src_e offset in
        current := Int.succ src_e;
        (*Format.printf "treat_itv: ib=%a ie=%a v=%a dib=%a die=%a@."
          Int.pretty (fst i) Int.pretty (snd i)
          V.pretty v
          Int.pretty (fst dest_i) Int.pretty (snd dest_i);*)
        add_map_internal dest_i new_vv acc
      in
      let acc = List.fold_right treat_interval concerned_itv _to in
      treat_empty_space acc (Int.succ stop)
    in
(*      Format.printf "Offsetmap_bitwise.copy_paste from:%a start:%a stop:%a start_to:%a to:%a result:%a@\n"
        (pretty) (Map from)
        Int.pretty start
        Int.pretty stop
        Int.pretty start_to
        (pretty) (Map _to)
        (pretty) (Map result);
*)
      result

  let copy_paste ~f from start stop start_to _to =
    match from, _to with
      Map from, Map _to -> Map (copy_paste_map ~f from start stop start_to _to)
    | _, _ ->
        let collapse_from = collapse from in
        let value_from =
         ( match f with
            Some (f,_default) ->
               (snd (f (true,collapse_from)))
        | None -> collapse_from )
        in
        Degenerate (V.join value_from (collapse _to))

  let copy_merge from start stop start_to _to =
    let old_value =
      copy_paste ~f:None
        _to start_to
        (Int.sub (Int.add start_to stop) start)
        start empty
    in
    let merged_value = join old_value from in
    copy_paste ~f:None merged_value start stop start_to _to

  let copy ~f from start stop =
    copy_paste ~f from start stop Int.zero empty

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
