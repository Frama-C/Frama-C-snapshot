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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please write a .mli and
   document it. *)

open Abstract_interp

module Unhashconsed_Int_Intervals = struct

  include Lattice_Interval_Set

  let fold_enum ~split_non_enumerable f v acc =
    ignore (split_non_enumerable);
    ignore (f);
    ignore (v);
    ignore (acc);
    assert false

  let pretty_typ typ fmt i =
    let typ =
      match typ with
        Some t -> t
      | None ->
          Cil_types.TArray
            (Cil_types.TInt(Cil_types.IUChar,[]),
             Some (Cil.kinteger64
                     ~loc:(Cil.CurrentLoc.get ())
                     Cil_types.IULongLong
                     (My_bigint.of_int64 922337203685477580L)
                     (* See Cuoq for rational *)),
             Cil.empty_size_cache (),
             [])
    in
    match i with
    | Top -> Format.fprintf fmt "[..]"
    | Set s ->
        if s=[] then Format.fprintf fmt "BottomISet"
        else begin
          let pp_one fmt (b,e)=
            assert (Int.le b e) ;
            ignore (Bit_utils.pretty_bits typ
                      ~use_align:false
                      ~align:Int.zero
                      ~rh_size:Int.one
                      ~start:b ~stop:e fmt) in
          let pp_stmt fmt r = Format.fprintf fmt "%a;@ " pp_one r in
          match s with
            | [] -> Format.pp_print_string fmt "{}"
            | [r] -> pp_one fmt r
            | s ->
                Format.fprintf fmt "@[<hov 1>{" ;
                List.iter (pp_stmt fmt) s ;
                Format.fprintf fmt "}@]" ;
        end

  let from_ival_int ival int =
    let max_elt_int = Kernel.ArrayPrecisionLevel.get() in
    let max_elt = Int.of_int max_elt_int in
    let add_offset x acc = join (inject_one ~value:x  ~size:int) acc in
    match ival with
    | Ival.Top(None, _, _, _)
    | Ival.Top(_, None, _, _) | Ival.Float _ -> top
    | Ival.Top(Some mn, Some mx, _r, m) ->
        if Int.le m int
        then inject_one ~value:mn ~size:(Int.add (Int.sub mx mn) int)
        else
          let elts = Int.native_div (Int.sub mx mn) m in
          if Int.gt elts max_elt then begin
            (* too many elements to enumerate *)
            Kernel.result ~once:true ~current:true
              "more than %d(%a) elements to enumerate. Approximating."
              max_elt_int
              Int.pretty elts;
           top
          end else Int.fold add_offset ~inf:mn ~sup:mx ~step:m bottom
    | Ival.Set(s) ->
        Array.fold_right
          add_offset
          s
          bottom

  let from_ival_size ival size =
    match size with
    | Int_Base.Top -> top
    | Int_Base.Bottom -> assert false
    | Int_Base.Value int ->
        from_ival_int ival int

  let inject_zero_max size =
     match size with
    | Int_Base.Top -> top
    | Int_Base.Bottom -> assert false
    | Int_Base.Value int ->
        inject_one ~value:Int.zero  ~size:int

  let diff x y =
    if is_included x y then bottom else x

  let diff_if_one _ = assert false (* Not implemented yet. *)

  let shift_int64 x intervs =
    match intervs with
      Top -> top
    | Set l ->
        inject (List.map (fun (bi,ei) ->  (Int.add bi x,Int.add ei x)) l)

  let shift_ival intervs ival =
    match ival with
      Ival.Top _ | Ival.Float _ -> top
    | Ival.Set s ->
        Array.fold_right
          (fun x acc ->
             join acc (shift_int64 x intervs))
          s
          bottom
end

module Int_Intervals = struct

  type tt =
    { h:int;
      v: Unhashconsed_Int_Intervals.t;
      tag:int }

  type widen_hint = Unhashconsed_Int_Intervals.widen_hint

  exception Error_Bottom = Unhashconsed_Int_Intervals.Error_Bottom
  exception Error_Top = Unhashconsed_Int_Intervals.Error_Top

  let tag { tag=tag } = tag

  let pretty_debug fmt x = Unhashconsed_Int_Intervals.pretty fmt x.v
  let pretty = pretty_debug

  let hash_internal {h=h} = h

  let equal_internal {v=v;h=h} {v=v';h=h'} =
    h = h' && Unhashconsed_Int_Intervals.equal v v'

  let name = "int_intervals"

  module IntIntervalsHashtbl =
    Buckx.MakeBig
      (struct
         type t = tt
         let equal = equal_internal
         let hash = hash_internal
         let pretty = pretty
         let id = name
       end)

  let table = IntIntervalsHashtbl.create 139
  let current_tag = ref 0 ;;

  let wrap x =
    let tag = !current_tag in
    let new_i =
      { h = Unhashconsed_Int_Intervals.hash x;
        v = x;
        tag = tag}
    in
    let result = IntIntervalsHashtbl.merge table new_i in
    if result == new_i then current_tag := succ tag;
    result

(* initial values go here *)
  let top = wrap Unhashconsed_Int_Intervals.top
  let bottom = wrap Unhashconsed_Int_Intervals.bottom
(* end of initial values *)


  (* Purely for implementation purposes, nothing to do with the ordering
     induced by the underlying lattice *)
  let compare i1 i2 = Datatype.Int.compare i1.tag i2.tag

  include
    Datatype.Make
      (struct
         type t = tt
         let structural_descr =
           Structural_descr.t_record
             [| Structural_descr.p_int;
                Unhashconsed_Int_Intervals.packed_descr;
                Structural_descr.p_int |]
         let reprs = [ top; bottom ]
         let name = "Abstract_value.Int_Intervals"
         let compare = compare
         let equal = ( == )
         let copy = Datatype.undefined
         let hash x = x.h
         let rehash x = wrap x.v
         let internal_pretty_code = Datatype.undefined
         let pretty = pretty
         let varname = Datatype.undefined
         let mem_project = Datatype.never_any_project
       end)

  let fold_enum ~split_non_enumerable f v acc =
    Unhashconsed_Int_Intervals.fold_enum ~split_non_enumerable f v.v acc

  let diff_if_one _ = assert false

  let diff x y = wrap (Unhashconsed_Int_Intervals.diff x.v y.v)

  let cardinal_less_than x n =
    Unhashconsed_Int_Intervals.cardinal_less_than x.v n

  let splitting_cardinal_less_than ~split_non_enumerable x n =
    Unhashconsed_Int_Intervals.splitting_cardinal_less_than
      ~split_non_enumerable x.v n

  let meet x y = wrap (Unhashconsed_Int_Intervals.meet x.v y.v)
  let link x y = wrap (Unhashconsed_Int_Intervals.link x.v y.v)
  let join x y = wrap (Unhashconsed_Int_Intervals.join x.v y.v)
  let narrow x y = wrap (Unhashconsed_Int_Intervals.narrow x.v y.v)
  let widen wh x y = wrap (Unhashconsed_Int_Intervals.widen wh x.v y.v)

(*
 THERE IS ONLY ONE HASHCONSING TABLE FOR Int_intervals.
   IT IS SHARED BETWEEN PROJECTS
*)

  let cardinal_zero_or_one x =
    Unhashconsed_Int_Intervals.cardinal_zero_or_one x.v

  let intersects x y =
    Unhashconsed_Int_Intervals.intersects x.v y.v

  let is_included x y =
    Unhashconsed_Int_Intervals.is_included x.v y.v

  let is_included_exn x y =
    Unhashconsed_Int_Intervals.is_included_exn x.v y.v

  let inject i =
    wrap (Unhashconsed_Int_Intervals.inject i)

  let pretty_typ typ fmt x =
    Unhashconsed_Int_Intervals.pretty_typ typ fmt x.v

  let from_ival_size iv s =
    wrap (Unhashconsed_Int_Intervals.from_ival_size iv s)

  let fold f x acc =
    Unhashconsed_Int_Intervals.fold f x.v acc

  let is_top x = equal x top

  exception Not_a_set

  let project_set x =
    match x.v with
      Unhashconsed_Int_Intervals.Top -> raise Not_a_set
    | Unhashconsed_Int_Intervals.Set s -> s

  let inject_zero_max b =
    wrap (Unhashconsed_Int_Intervals.inject_zero_max b)

  let inject_bounds b e =
    wrap (Unhashconsed_Int_Intervals.inject_bounds b e)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
