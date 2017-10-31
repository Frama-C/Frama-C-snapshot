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

open Abstract_interp
open Locations

(* Key used for logging everything related to str-builtins (not only strlen)*)
let dkey = Value_parameters.register_category "strlen"

(* for debugging *)
(*let callstack_base = ref 0
let reset_callstack_base () =
  let len = Printexc.raw_backtrace_length (Printexc.get_callstack 999) in
  callstack_base := len
let get_callstack_depth () : int =
  Printexc.raw_backtrace_length (Printexc.get_callstack 999) - !callstack_base*)
let fpf s =
  (*the callstack length can be used to indent debugging messages*)
  (*let prefix = String.make (get_callstack_depth ()) ' ' in*)
  Value_parameters.printf ~dkey s

module I (* alias for Integer, for better readability *) = struct
  let ( = ) = Integer.equal
  let ( <> ) x y = not (Integer.equal x y)
  let ( < ) = Integer.lt
  let ( > ) = Integer.gt
(*  let ( <= ) = Integer.le*)
  let ( >= ) = Integer.ge
  let ( + ) = Integer.add
  let ( - ) = Integer.sub
  let ( * ) = Integer.mul
  let ( / ) = Integer.pos_div
  let ( % ) = Integer.pos_rem
  let zero = Integer.zero
  let one = Integer.one
  let minus_one = Integer.minus_one
  let eight = Integer.eight
  let succ = Integer.succ
  let pred = Integer.pred
  let of_int = Integer.of_int
  let min = Integer.min
  let max = Integer.max
  (* Returns the minimum value of an Ival. Requires a non-bottom Ival
     having a bounded minimum value. *)
  let the_min ival = Extlib.the (fst (Ival.min_and_max ival))
  (* Returns the maximum value of an Ival. Requires a non-bottom Ival
     having a bounded maximum value. *)
(*  let the_max ival = Extlib.the (snd (Ival.min_and_max ival))*)
end

module VU = Cvalue.V_Or_Uninitialized

(* Init_status describes if a given char is:
   - always initialized or maybe uninitialized;
   - always non-escaping or maybe escaping;
   - always determinate or maybe indeterminate (out of bounds). *)
module Init_status = struct
  type t = {
    maybe_uninit : bool;
    maybe_esc : bool;
    maybe_indet : bool;
  }
  let hash (is : t) = Hashtbl.hash is
  let bottom = { maybe_uninit = false; maybe_esc = false; maybe_indet = false }
  let top = { maybe_uninit = true; maybe_esc = true; maybe_indet = true }
  let join is1 is2 =
    let maybe_uninit = is1.maybe_uninit || is2.maybe_uninit in
    let maybe_esc = is1.maybe_esc || is2.maybe_esc in
    let maybe_indet = is1.maybe_indet || is2.maybe_indet in
    { maybe_uninit; maybe_esc; maybe_indet }
  let narrow is1 is2 =
    let maybe_uninit = is1.maybe_uninit && is2.maybe_uninit in
    let maybe_esc = is1.maybe_esc && is2.maybe_esc in
    let maybe_indet = is1.maybe_indet && is2.maybe_indet in
    { maybe_uninit; maybe_esc; maybe_indet }

  (* sets [maybe_indet] to true, without modifying the other flags *)
  let may_indet is =
    { maybe_uninit = is.maybe_uninit; maybe_esc = is.maybe_esc;
      maybe_indet = true }

  (* if [is] is currently "clean" (initialized/non-escaping), then
     add [maybe_indet] as error, otherwise do not modify it. *)
  let ensure_an_error is =
    if not (is.maybe_uninit && is.maybe_esc) then may_indet is
    else is

  let pretty fmt is =
    match is.maybe_uninit, is.maybe_esc, is.maybe_indet with
    | false, false, false -> Format.fprintf fmt "INIT/NO_ESC"
    | false, false, true  -> Format.fprintf fmt "INIT/NO_ESC/MAYBE_INDET"
    | false, true, false  -> Format.fprintf fmt "INIT/ESC"
    | false, true, true   -> Format.fprintf fmt "INIT/ESC/MAYBE_INDET"
    | true, false, false  -> Format.fprintf fmt "UNINIT/NO_ESC"
    | true, false, true   -> Format.fprintf fmt "UNINIT/NO_ESC/MAYBE_INDET"
    | true, true, false   -> Format.fprintf fmt "UNINIT/ESC"
    | true, true, true    -> Format.fprintf fmt "UNINIT/ESC/MAYBE_INDET"

  (* creates a new Init_status, copying flags from [vu] and with initial
     [maybe_indet]. *)
  let of_vu vu maybe_indet =
    let maybe_uninit = not (VU.is_initialized vu) in
    let maybe_esc = not (VU.is_noesc vu) in
    { maybe_uninit; maybe_esc; maybe_indet }
end
module IS = Init_status

(* [FS] (for "found status") resumes, for each char, whether the searched
   character is present (always/maybe/never/invalid access).
   Used both for the searched character and the stop character
   (esp. in strchr). *)
module FS = struct
  include Bool
  let pretty fmt = function
    | Bool.Top -> Format.fprintf fmt "Maybe"
    | Bool.True -> Format.fprintf fmt "Must"
    | Bool.False -> Format.fprintf fmt "Non"
    | Bool.Bottom -> Format.fprintf fmt "Invalid"
end

(* [char_status] resumes the initialization and found/not found status
   for each char in the simplified Charcharmap. *)
module Char_status = struct
  type t = { search_st: FS.t; stop_st: FS.t; init_st: IS.t }
  let bottom = { search_st = FS.bottom; stop_st = FS.bottom; init_st = IS.bottom }
  let top = { search_st = FS.top; stop_st = FS.top; init_st = IS.top }
  let equal bs1 bs2 =
    bs1.search_st = bs2.search_st
    && bs1.stop_st = bs2.stop_st
    && bs1.init_st = bs2.init_st
  let join bs1 bs2 = {
    search_st = FS.join bs1.search_st bs2.search_st;
    stop_st = FS.join bs1.stop_st bs2.stop_st;
    init_st = IS.join bs1.init_st bs2.init_st;
  }
  let is_included bs1 bs2 = equal (join bs1 bs2) bs2
  let narrow bs1 bs2 = {
    search_st = FS.narrow bs1.search_st bs2.search_st;
    stop_st = FS.narrow bs1.stop_st bs2.stop_st;
    init_st = IS.narrow bs1.init_st bs2.init_st;
  }
  let hash bs =
    17 * FS.hash bs.search_st + 23 * FS.hash bs.stop_st + 31 * IS.hash bs.init_st
  let pretty fmt bs =
    Format.fprintf fmt "{search_st:%a,stop_st:%a,init_st:%a}"
      FS.pretty bs.search_st FS.pretty bs.stop_st IS.pretty bs.init_st
end
module BS = Char_status

(* Datatype used to construct [Charcharmap]. *)
module Str_datatype = struct
  (* Definitions for datatype *)
  type t = Char_status.t
  let hash = Char_status.hash
  let name = "Builtins_string.Str_datatype"
  let rehash = Datatype.identity
  let structural_descr = Structural_descr.t_abstract
  let reprs = [Char_status.bottom]
  let equal t1 t2 = Char_status.equal t1 t2
  let compare t1 t2 = compare t1 t2
  let copy = Datatype.identity
  let internal_pretty_code = Datatype.undefined
  let pretty = Char_status.pretty
  let varname t = "str_" ^ (Pretty_utils.sfprintf "%a" pretty t)
  let mem_project _ _ = false
end
module Str_lattice = struct
  module M = Datatype.Make(Str_datatype)
  include M
  module BS = Char_status
  let join = BS.join
  let is_included = BS.is_included
  let bottom = BS.bottom
  let top = BS.top
  let narrow = BS.narrow
end
module SL = Str_lattice
module Charcharmap = Offsetmap.Make_bitwise(Str_lattice)

(* Boolean-like flag to indicate if the built-in is imprecise for the given
   arguments. A future version may remove this restriction. *)
type imprecise_builtin =
  | Imprecise
  | Not_imprecise

let cwidth_to_bytes cwidth = Int.(div cwidth eight)

(* converts bits to chars, emitting a warning in case of inexact division. *)
let chars_of_bits ~cwidth ?inexact i =
  if I.(i % cwidth <> zero) &&
  match inexact with | Some b -> not b | None -> true then
    (* message for debugging purposes mostly, should not happen *)
    Value_parameters.warning "chars_of_bits: inexact division (%a / %a)"
      Int.pretty i Int.pretty cwidth;
  I.(i / cwidth)
let bits_of_chars ~cwidth i = I.(i * cwidth)
let is_char_aligned ~cwidth i = I.(i % cwidth = zero)

(* Given a value from a charcharmap, returns its found_status.
   Special case for '\0' (more efficient, used by strlen/strnlen). *)
let found_status_of_v_zero ival =
  assert(not (Ival.is_bottom ival));
  if (Ival.contains_zero ival) then
    if (not (Ival.contains_non_zero ival)) then FS.True
    else FS.Top
  else FS.False

(* Given a value from a charcharmap, returns its char_status.
   Searches for the character(s) in [chr] (memchr). *)
(* requires [not (Ival.is_bottom ival) *)
let found_status_of_v_char chr ival =
  assert(not (Ival.is_bottom ival));
  if Ival.intersects chr ival then
    (* maybe or must *)
    if Ival.cardinal_zero_or_one ival && Ival.equal chr ival then FS.True
    else FS.Top
  else FS.False

let zero_zero_bs_of_vu vu : BS.t =
  let init_st = IS.of_vu vu false in
  let cv = VU.get_v vu in
  if Cvalue.V.is_bottom cv then
    { BS.search_st = FS.Bottom; stop_st = FS.Bottom; init_st }
  else
    let ival = Cvalue.V.project_ival cv in
    let search_st = found_status_of_v_zero ival in
    { BS.search_st; stop_st = FS.False (*same as search_st*); init_st }

let char_char_bs_of_vu chr vu : BS.t =
  let init_st = IS.of_vu vu false in
  let cv = VU.get_v vu in
  if Cvalue.V.is_bottom cv then
    { BS.search_st = FS.Bottom; stop_st = FS.Bottom; init_st }
  else
  let ival = Cvalue.V.project_ival cv in
  let search_st = found_status_of_v_char chr ival in
  { BS.search_st; stop_st = FS.False (*same as search_st*); init_st }

let char_zero_bs_of_vu chr vu : BS.t =
  let init_st = IS.of_vu vu false in
  let cv = VU.get_v vu in
  if Cvalue.V.is_bottom cv then
    { BS.search_st = FS.Bottom; stop_st = FS.Bottom; init_st }
  else
  let ival = Cvalue.V.project_ival cv in
  let search_st = found_status_of_v_char chr ival in
  let stop_st =
    if Ival.equal chr Ival.zero then FS.False
    else found_status_of_v_zero ival
  in
  { BS.search_st; stop_st; init_st }

(* [add_char_status validity offsets bs bcm] binds char status [bs] to
   offsets [offsets] in [bcm]. *)
let add_char_status ~validity offsets bs bcm =
  let size = Int_Base.one in
  let exact = true in
  match Charcharmap.add_binding_ival ~validity ~exact offsets ~size bs bcm with
  | `Bottom -> assert false
  | `Value m -> m

(* [process_range_whole] efficiently processes one entire range of the offsetmap
   (between [range_start] and [range_end]), but requires its values to be
   char_aligned. Also handles isotropic values. *)
let process_range_whole ~cwidth bs_of_vu_f offsetmap offsm_validity base_size_chars range_start range_end vu size_bits acc =
  let bcm_validity (*validity/cwidth*) = Base.validity_from_size base_size_chars in
  if I.(size_bits = one) then (* isotropic value *)
    let bs = bs_of_vu_f vu in
    let offsets =
      Ival.scale_div ~pos:true cwidth
        (Ival.inject_range (Some range_start) (Some range_end))
    in
    add_char_status ~validity:bcm_validity offsets bs acc
  else begin
    let acc = ref acc in
    let nb_chars_val = I.(size_bits / cwidth) in
    (* in this range of [nb_repeat+1] values, each composed of
       [nb_chars_val] chars, write the first char for each repetition
       in the range, then the second char, then the third, etc.

       Example: the offsetmap below has three ranges, and we are
       processing the second one (each underscore is a char, values
       are separated by vertical bars; cwidth is assumed to be 8, i.e.
       non-wide characters):

                   abc? abc? abc?
       |________| |____|____|____| |____|____|
                  ^              ^
                  first_bit      last_bit

       This range has size 8 * 12 = 96 bits, composed of three
       repetitions of the chars "abc?" (where "?" is unknown).
       The result we want in the end is the following, where
       'F' (for False) is "Non" and 'U' (for Unknown) is "Maybe":

                   FFFU FFFU FFFU
       |________| |____|____|____| |____|____|
                   ^         ^
       for i=0:  start_char  stop_char

                    ^         ^
       for i=1:  start_char  stop_char

       Each value occupies a single bit of the resulting charcharmap.
       In this example, [size_bits] equals 32, [nb_chars_val] equals 4,
       [range_end] equals [159] (64+95), [range_start] equals
       [64] (159-95), [nb_repeat] equals 2 (truncated division;
       it may be 0 if there are no repetitions), [start_char] equals
       8, and [stop_char] equals 16.

       If nb_repeat is too high (above plevel), the result is
       automatically approximated. *)
    for i = 0 to (Int.to_int nb_chars_val) - 1 do
      let bs =
        let cur_start_bits = I.(range_start + (of_int i * cwidth)) in
        let offsets = Ival.inject_singleton cur_start_bits in
        let _, vu =
          Cvalue.V_Offsetmap.find ~validity:offsm_validity ~offsets ~size:cwidth offsetmap
        in
        bs_of_vu_f vu
      in
      let start_char = I.(of_int i + range_start / cwidth) in
      (* nb_repeat is intentionally truncating the division below
         (may be 0). *)
      let nb_repeat = I.((range_end - range_start) / size_bits) in
      let stop_char = I.(start_char + nb_repeat * nb_chars_val) in
      let offsets =
        Ival.inject_interval (Some start_char) (Some stop_char)
          (Int.rem start_char nb_chars_val) nb_chars_val
      in
      (* in some cases (notably when the range does not start at remainder 0)
         offsets may include values beyond the size of the offsetmap,
         so they are filtered to avoid [add_char_status] from trying to
         retrieve them and obtaining `Bottom. If the filtered offset is empty,
         then [add_char_status] is not called. *)
      let filtered_offsets = Ival.backward_comp_int_left Comp.Lt offsets
          (Ival.inject_singleton base_size_chars)
      in
      if not (Ival.is_bottom filtered_offsets) then
        acc := add_char_status ~validity:bcm_validity filtered_offsets bs !acc
    done;
    !acc
  end

(* [process_range_charwise] splits a given offsetmap range into each char,
   and then iterates char-per-char. Less efficient than [process_range_whole],
   but necessary when the values are not char-aligned (e.g. due to bitfields). *)
let process_range_charwise ~cwidth bs_of_vu_f offsetmap offsm_validity base_size_chars range_start range_end acc =
  let bcm_validity (*validity/cwidth*) = Base.validity_from_size base_size_chars in
  let acc = ref acc in
  for i = Int.to_int (chars_of_bits ~cwidth ~inexact:true range_start) to
      Int.to_int (chars_of_bits ~cwidth ~inexact:true range_end)
  do
    let bs =
      let cur_start_bits = I.(of_int i * cwidth) in
      let offsets = Ival.inject_singleton cur_start_bits in
      let _, vu =
        Cvalue.V_Offsetmap.find ~validity:offsm_validity ~offsets ~size:cwidth offsetmap
      in
      bs_of_vu_f vu
    in
    let offsets = Ival.inject_singleton (I.of_int i) in
    acc := add_char_status ~validity:bcm_validity offsets bs !acc
  done;
  !acc

(* Computes a [Charcharmap.t] from a given base and its offsetmap.
   The resulting map associates, to each char offset in the base,
   a status maybe/must/not indicating whether the searched character
   can be found at that position.
   [first_offset_bits] and [last_offset_bits] are an optimization:
   instead of computing the map for the entire base, it is computed
   only between these bits. [last_offset_bits] must not be greater
   than the end of the base ([base_end_bits]).
   [base_end_bits] is used to compute the base length. *)
let make_charcharmap ~cwidth bs_of_vu_f base m first_offset_bits last_offset_bits base_end_bits =
  assert Int.(le last_offset_bits base_end_bits);
  (* [validity] is the validity of the base (original offsetmap), while
     [validitychar] is the validity of the new offsetmap that will be created *)
  let validity = Base.validity base in
  let base_size_chars = chars_of_bits ~cwidth I.(succ base_end_bits) in
  let bot = Charcharmap.create ~size:base_size_chars SL.bottom (* TODO *) in
  Cvalue.V_Offsetmap.fold_between ~entire:false
    (first_offset_bits, last_offset_bits)
    (fun (range_start, range_end) (vu, size_bits, rel) acc ->
       if is_char_aligned ~cwidth range_start && is_char_aligned ~cwidth I.(succ range_end) &&
          ((is_char_aligned ~cwidth (Obj.magic (*TODO*) rel) && is_char_aligned ~cwidth size_bits
            || I.(size_bits = one) )) then
         (* linear in [size_bits] in some cases, or in [plevel] at most *)
         process_range_whole ~cwidth bs_of_vu_f m validity base_size_chars
           range_start range_end vu size_bits acc
       else
         (* code with bitfields; linear in the size of the range *)
         process_range_charwise ~cwidth bs_of_vu_f m validity base_size_chars
           range_start range_end acc
    ) m bot

(* Problems which can be diagnosed by str-related builtins. *)
module Problem = struct
  type t =
    | Init of bool
    | Esc of bool
    | Indet of bool
    | Maybe_invalid
    | Base of Base.t
    | Misc of string

  (* [compare] is based on [Pervasives.compare], but for better clarity during
     pretty-printing, we ensure [Misc] is smaller than all other elements *)
  let compare p1 p2 = match p1, p2 with
    | Misc s1, Misc s2 -> String.compare s1 s2
    | Misc _, _ -> -1
    | _, Misc _ -> 1
    | _, _ -> Pervasives.compare p1 p2

  let pretty fmt = function
    | Init (sure) ->
      Format.fprintf fmt "%suninitialized values"
        (if sure then "" else "possible ")
    | Esc (sure) ->
      Format.fprintf fmt "%sescaping addresses"
        (if sure then "" else "possible ")
    | Indet (sure) ->
      Format.fprintf fmt "%sreading indeterminate data"
        (if sure then "" else "possibly ")
    | Maybe_invalid ->
      Format.fprintf fmt "possibly reading invalid memory"
    | Base base ->
      Format.fprintf fmt "invalid base: %a" Base.pretty base
    | Misc msg ->
      Format.fprintf fmt "%s" msg

end

module Problems = struct
  include Set.Make(struct
      type t = Problem.t
      let compare = Problem.compare
    end)
  (* [weaken] turns "definitive problems" into "maybe problems" *)
  let weaken s =
    fold (fun p acc ->
        if p = Problem.Init true then add (Problem.Init false) acc
        else if p = Problem.Esc true then add (Problem.Esc false) acc
        else if  p = Problem.Indet true then add (Problem.Indet false) acc
        else acc
      ) empty s

  let pretty fmt s =
    (* normalize to avoid printing twice for definite/possible problems *)
    let normalize s =
      let s = if mem (Problem.Init false) s && mem (Problem.Init true) s then
          remove (Problem.Init true) s
        else s
      in
      let s = if mem (Problem.Esc false) s && mem (Problem.Esc true) s then
          remove (Problem.Esc true) s
        else s
      in
      let s = if mem (Problem.Indet false) s && mem (Problem.Indet true) s then
          remove (Problem.Indet true) s
        else s
      in
      s
    in
    Format.fprintf fmt "@[<v>%a@]"
      (Pretty_utils.pp_list ~pre:"" ~suf:"" ~sep:"@," Problem.pretty)
      (elements (normalize s))
end

module BaseMap = Map.Make(struct
    type t = Base.t
    let compare b1 b2 = Base.compare b1 b2
  end)

(* Builds a map from bases to offsets *)
let basemap_of_locmap (m : Cvalue.V.M.t) : Ival.t BaseMap.t =
  Cvalue.V.M.fold (fun base offs acc ->
      BaseMap.add base offs acc
    ) m BaseMap.empty

type exhausted_status = Maybe_exhausted of Int.t (*relative*)
                      | Non_exhausted
let pp_exh_st fmt = function
  | Maybe_exhausted rel_i -> Format.fprintf fmt "Maybe_exhausted(%a)"
                                 Int.pretty rel_i
  | Non_exhausted -> Format.fprintf fmt "Non_exhausted"
let concatenate_exh es len opt_n_len =
  match opt_n_len with
  | None -> Non_exhausted
  | Some n_len ->
    assert (not (Ival.is_bottom n_len));
    let n_min = Extlib.the (fst (Ival.min_and_max n_len)) in
    if I.(len < n_min) then Non_exhausted
    else
      match es with
      | Maybe_exhausted rel_i -> Maybe_exhausted I.(rel_i + len)
      | Non_exhausted -> Maybe_exhausted len
let join_exh es1 es2 = match es1, es2 with
  | Maybe_exhausted rel_i1, Maybe_exhausted rel_i2 ->
    Maybe_exhausted (I.max rel_i1 rel_i2)
  | Maybe_exhausted rel_i, Non_exhausted
  | Non_exhausted, Maybe_exhausted rel_i -> Maybe_exhausted rel_i
  | Non_exhausted, Non_exhausted -> Non_exhausted

module Base_res = struct
  type t = {
    vals: Ival.t;
    abs_offs: Ival.t;
    char_fs: FS.t;
    maybe_not_found: bool;
    es: exhausted_status;
    problems: Problems.t;
  }
  let bottom = {vals = Ival.bottom; abs_offs = Ival.bottom; char_fs = FS.Bottom;
                maybe_not_found = false; es = Non_exhausted; problems = Problems.empty}
  let join br1 br2 : t =
    let vals = Ival.join br1.vals br2.vals in
    let abs_offs = Ival.join br1.abs_offs br2.abs_offs in
    let char_fs = FS.join br1.char_fs br2.char_fs in
    let maybe_not_found = br1.maybe_not_found || br2.maybe_not_found in
    let es = join_exh br1.es br2.es in
    let problems = Problems.union br1.problems br2.problems in
    {vals; abs_offs; char_fs; maybe_not_found; es; (*has_null_result;*) problems}
  let pretty fmt br =
    Format.fprintf fmt "@[{vals:%a,abs_offs:%a,fs:%a,es:%a,problems:%a}@]"
      Ival.pretty br.vals Ival.pretty br.abs_offs FS.pretty br.char_fs
      pp_exh_st br.es
      Problems.pretty br.problems
end
module BR = Base_res

type bm_res_t = Base_res.t BaseMap.t

type opt_int_pair = (Int.t * Int.t) option

let ival_of_opt_int_pair = function
  | None -> Ival.bottom
  | Some (i1, i2) -> Ival.inject_range (Some i1) (Some i2)

(*let join_opt_int_pair p1 p2 =
  match p1, p2 with
  | None, None -> None
  | None, Some _ -> p2
  | Some _, None -> p1
  | Some (mn1, mx1), Some (mn2, mx2) ->
    Some (Int.min mn1 mn2, Int.max mx1 mx2)*)

(* Result of intermediate str-related functions.
   [Maybe_ok(bounds,abs_offs,char_fs,maybe_found_stop,is)] indicates that there
   exists at least one (possibly) valid result (including "not found").
   [bounds] overapproximates the final result for length functions.
   [abs_offs] overapproximates the min/max absolute offsets for pointer functions.
   [char_fs] is summarizes the
   presence of the searched character and is used by some callers.
   [maybe_found_stop] is used to indicate, in the case where [bounds] is [None],
   whether the stopping character has possibly been found *before* the searched
   character (which implies a valid "not found" result).
   [is] indicates the possibility of errors (initialization, danglingness,
   invalid access).
   [Never_ok(is)] indicates that no valid solution exists (the function always
   fails). [is] contains the error(s) that occur (it should never be
   [C_init_noesc] with [maybe_indet = false] in this case). *)
type str_res_t =
  | Maybe_ok of Ival.t (*bounds (relative offsets)*)
                * Ival.t (*abs_offs (absolute offsets)*)
                * FS.t (*char_fs*)
                * bool (*maybe_found_stop*)
                * exhausted_status
                * IS.t
  | Never_ok of IS.t (* init_status; init/noesc with may_indet=false
                        is a nonsensical value here *)

let pp_opt_int_pair =
  Pretty_utils.pp_opt (fun fmt (a,b) -> Format.fprintf fmt "@[%a,%a@]"
                          Int.pretty a Int.pretty b)

let pp_str_res_t fmt = function
  | Maybe_ok (bounds,abs_offs,char_fs,maybe_found_stop,es,is) ->
    Format.fprintf fmt "@[Maybe_ok(%a,%a,%a,%b,%a,%a)@]" Ival.pretty bounds
      Ival.pretty abs_offs FS.pretty char_fs maybe_found_stop pp_exh_st es
      IS.pretty is
  | Never_ok (is) -> Format.fprintf fmt "@[Never_ok(%a)@]"
                         IS.pretty is

(* Types and auxiliary functions related to the 'exact' function which computes,
   for a single offset, the result of a search function on a given base.
   This function is used as-is, in the case of a small set of offsets, and also
   as first step of the imprecise, interval-based RTL search.
   Note that the [n] argument of a limited-range search is not necessarily a
   singleton. *)
module Search_single_offset = struct
  (* accumulator for [search] *)
  type single_acc_t = {
    kars_pos: opt_int_pair (* leftmost *possible* searched character
                              (abbreviated as 'kar') found so far,
                              and rightmost *possible* kar that is not
                              preceded by a definitive kar *);
    maybe_found_stop: bool; (* maybe found stopping character BEFORE kar *)
    is: IS.t;
    fuel_left: opt_int_pair (* None => unbounded fuel *);
  }

  let pp_acc_t fmt acc =
    Format.fprintf fmt "@[{acc_singleton_t:kars_pos=%a,fuel_left=%a,is=%a}@]"
      pp_opt_int_pair acc.kars_pos pp_opt_int_pair acc.fuel_left IS.pretty acc.is

  (* Exceptions used to speed up the analysis:
     - [Must_stop] indicates the either the searched character or the stopped
       character have definitely been found, so the search may stop.
     - [Maybe_no_fuel] indicates that the search cannot continue (either due to
       reaching the end of the base, or due to a completely uninitialized value),
       but at least one possible solution has been found.
     - [Bottom_val] indicates that no possible solution has been found and the
       search must stop. The only case where this does not lead to an error is
       when a limited search (strnlen/memchr) has run out of "fuel" (and should
       return the number of searched chars). *)
  exception Must_stop of (Int.t * Int.t) option (*bounds*)
                         * FS.t (*Found_status of searched character*)
                         * bool (*maybe_found_stop*)
                         * IS.t (*init status*)
  exception Maybe_no_fuel of (Int.t * Int.t) option (*bounds*)
                             * bool (*maybe_found_stop*)
                             * Int.t (*rightmost contiguous valid char*)
                             * IS.t
                             * bool (*maybe_had_fuel*)
  exception Bottom_val of (Int.t * Int.t) option
                          * bool (*maybe_found_stop*)
                          * IS.t
                          * Int.t (*range_start*)

  (* Initializes the fuel counters, converting unbounded fuel to a sufficiently
     large (finite) value. *)
  let init_fuel_from_n_len n_len max_char_to_look =
    (* infinite_fuel is a value sufficiently high that should never reach zero
       before the iteration ends *)
    let infinite_fuel = Int.succ max_char_to_look in
    match n_len with
    | None -> None
    | Some n ->
      match Ival.min_and_max n with
      | None, None -> Some (Int.zero, infinite_fuel)
      | Some min, None -> Some (min, infinite_fuel)
      | None, Some max -> Some (Int.zero, max)
      | Some min, Some max -> Some (min, max)

  (* Consumes [range_end - range_start + 1] units of fuel, updating minimum and
     maximum counters accordingly. Note that [min_fuel < 0] is OK, but
     [max_fuel < 0] should never happen. *)
  let consume_fuel fuel range_start range_end =
    Extlib.opt_bind
      (fun (min_f, max_f) ->
         let new_min = I.(max (min_f - (range_end + one - range_start)) zero) in
         let new_max = I.(max_f - (range_end + one - range_start)) in
         if I.(new_max < zero) then assert false
         else Some (new_min, new_max)
      ) fuel

  (* [range_start] and [range_end] are in chars *)
  let process_search_in_char range_start range_end bs acc =
    match bs.BS.search_st with
    | FS.Bottom ->
      (* this Invalid can only be due to initialization/danglingness;
         check if min_fuel has been reached (for strnlen), to find out
         which exception to raise *)
      begin
        assert (bs.BS.stop_st = FS.Bottom);
        let maybe_found_stop = false in
        match acc.fuel_left with
        | Some (min_f, max_f) when I.(min_f = zero) ->
          (* min fuel has been consumed, strnlen has valid results *)
          raise (Maybe_no_fuel (acc.kars_pos, maybe_found_stop,
                                I.(pred range_start),
                                IS.may_indet acc.is, I.(max_f > zero)))
        | _ ->
          (* unbounded or excess min fuel => sure error *)
          fpf "reached end of base with excess fuel \
               (fuel_left = %a, offset was <N/A>, max_char_to_look <N/A>): \
               Bottom_val, acc: %a" pp_opt_int_pair acc.fuel_left
            (*Int.pretty offset Int.pretty max_char_to_look*) pp_acc_t acc;
          let maybe_found_stop = false in
          raise (Bottom_val (acc.kars_pos, maybe_found_stop,
                             acc.is, range_start))
      end
    | FS.True ->
      begin
        (* searched character was found, and because it is prioritary,
           maybe_found_stop is [Non] *)
        let maybe_found_stop = false in
        match acc.kars_pos with
        | None ->
          raise (Must_stop
                   (Some (range_start, range_start), FS.True,
                    maybe_found_stop, IS.join acc.is bs.BS.init_st))
        | Some (lz, _rz) ->
          raise (Must_stop
                   (Some (lz, range_start), FS.True,
                    maybe_found_stop, IS.join acc.is bs.BS.init_st))
      end
    | FS.False ->
      begin
        (* searched character not found, look for stopping character *)
        match bs.BS.stop_st with
        | FS.True ->
          fpf "found stopping character (must)!";
          let maybe_found_stop = true in
          raise (Must_stop (acc.kars_pos, FS.False, maybe_found_stop,
                            IS.join acc.is bs.BS.init_st))
        | FS.Top ->
          (* possible stop character; continue *)
          { kars_pos = acc.kars_pos;
            maybe_found_stop = true;
            fuel_left = consume_fuel acc.fuel_left range_start range_end;
            is = IS.join acc.is bs.BS.init_st }
        | FS.False ->
          { kars_pos = acc.kars_pos;
            maybe_found_stop = acc.maybe_found_stop;
            fuel_left = consume_fuel acc.fuel_left range_start range_end;
            is = IS.join acc.is bs.BS.init_st }
        | FS.Bottom -> (* impossible case *) assert false
      end
    | FS.Top ->
      let kars_pos = match acc.kars_pos with
        | None -> (range_start, range_end)
        | Some (lz, _rz) -> (lz, range_end)
      in
      (* searched character possibly not found, look for stop character *)
      begin
        match bs.BS.stop_st with
        | FS.True ->
          fpf "found stopping character (must)!";
          let maybe_found_stop = true in
          (*TODO: more precise tests to rule out "maybe found char" *)
          raise (Must_stop (Some kars_pos, FS.Top, maybe_found_stop,
                            IS.join acc.is bs.BS.init_st))
        | FS.Top ->
          (* possible stop character; continue *)
          { kars_pos = Some kars_pos;
            maybe_found_stop = true;
            fuel_left = consume_fuel acc.fuel_left range_start range_end;
            is = IS.join acc.is bs.BS.init_st }
        | FS.False ->
          { kars_pos = Some kars_pos;
            maybe_found_stop = acc.maybe_found_stop;
            fuel_left = consume_fuel acc.fuel_left range_start range_end;
            is = IS.join acc.is bs.BS.init_st }
        | FS.Bottom -> (* impossible case *) assert false
      end

  (* fuel = None => unbounded fuel (strlen, or strnlen with unbounded argument)
     fuel = (Some n_min, Some n_max) =>
     can look only up to [n_max] chars (does not include offset),
     and must look at least [n_min] chars *)
  (* Performs a left-to-right search starting at a fixed offset.
     Takes into account a possibly variable and possibly unbounded amount of
     fuel (search distance). *)
  let search charcharmap offset n_len max_char_to_look : str_res_t =
    fpf "search (single offset): offset: %a, n_len: %a, max_char_to_look: %a"
      Int.pretty offset (Pretty_utils.pp_opt Ival.pretty) n_len
      Int.pretty max_char_to_look;
    let fuel = init_fuel_from_n_len n_len max_char_to_look in
    fpf "search (single offset): init_fuel = %a" pp_opt_int_pair fuel;
    let init_acc = { kars_pos = None; maybe_found_stop = false;
                     fuel_left = fuel; is = IS.bottom; }
    in
    (* Note: min and max bounds are computed using absolute values
       (instead of relative ones) during the fold, and only at the end
       they are converted into relative bounds. *)
    try
      if Int.(lt offset zero) then
        raise (Bottom_val (None, false, IS.bottom, offset));
      let acc =
        Charcharmap.fold_itv ~direction:`LTR ~entire:false
          (fun (range_start, range_end) bs acc ->
             fpf "  fold_itv in search (single offset): range_offsets: \
                  %a - %a, acc = %a, bs: %a" Int.pretty range_start
               Int.pretty range_end pp_acc_t acc Str_datatype.pretty bs;
             process_search_in_char range_start range_end bs acc
          ) (offset, max_char_to_look) charcharmap init_acc
      in
      match acc.fuel_left with
      | Some (min_f, max_f) when I.(min_f = zero) ->
        (* min fuel has been consumed *)
        fpf "reached end of base, but no excess fuel acc: %a" pp_acc_t acc;
        raise (Maybe_no_fuel (acc.kars_pos, acc.maybe_found_stop, max_char_to_look,
                              acc.is, I.(max_f > zero)))
      | _ ->
        (* unbounded or excess fuel, but reached end of base => invalid access *)
        fpf "reached end of base with excess fuel \
             (fuel_left = %a, offset was %a, max_char_to_look %a): \
             Bottom_val, acc: %a"
          pp_opt_int_pair acc.fuel_left Int.pretty offset
          Int.pretty max_char_to_look pp_acc_t acc;
        raise (Bottom_val (acc.kars_pos, acc.maybe_found_stop,
                           acc.is, I.succ max_char_to_look))
    with
    | Must_stop (kars_pos, char_fs, maybe_found_stop, is) ->
      (* certainly found the searched or stopping character, will stop searching *)
      (* char_fs may be Must (if found searched character),
         or Maybe/None (if found stopping character).
         if char_fs = Maybe/None, then there is a possible "not found" result. *)
      fpf "Must_stop";
      let res_ival = match kars_pos with
        | None -> Ival.bottom
        | Some (lz, rz) ->
            Ival.inject_range (Some I.(lz - offset)) (Some I.(rz - offset))
      in
      Maybe_ok (res_ival, ival_of_opt_int_pair kars_pos, char_fs,
                maybe_found_stop, Non_exhausted, is)
    | Maybe_no_fuel (kars_pos, maybe_found_stop, rcvb, is, maybe_had_fuel) ->
      fpf "Maybe_no_fuel, kars_pos: %a, rightmost contiguous valid char: %a, \
           maybe_had_fuel: %b" pp_opt_int_pair kars_pos Int.pretty rcvb
        maybe_had_fuel;
      begin
        match kars_pos with
        | None -> (* strn* with no kars_pos: return max_n *)
          fpf "strn* with no kars_pos (exhausted n_len)";
          let res = Ival.bottom in
          let exh_n = I.(rcvb - offset + one) in
          let new_is = if maybe_had_fuel then IS.may_indet is else is in
          let char_fs = FS.False in
          Maybe_ok (res, Ival.bottom, char_fs, maybe_found_stop, Maybe_exhausted exh_n, new_is)
        | Some (lz, rz) ->
          fpf "strn* with kars_pos (exhausted n_len)";
          (* note: there may be holes between the previous max and the
             n_len exhaustion, therefore they are added in separate steps
             (a single inject_range would lose precision) *)
          let min = I.(lz - offset) in
          let max = I.(rz - offset) in
          let abs_offs = Ival.inject_range (Some lz) (Some rz) in
          let bounds_without_exhaustion = Ival.inject_range (Some min) (Some max) in
          let max_from_n = I.(rcvb - offset + one) in
          fpf "max_from_n: %a (rcvb: %a, offset: %a)"
            Int.pretty max_from_n Int.pretty rcvb Int.pretty offset;
          let bounds = bounds_without_exhaustion in
          let new_is = if maybe_had_fuel then IS.may_indet is else is in
          let char_fs = FS.Top in
          Maybe_ok (bounds, abs_offs, char_fs, maybe_found_stop,
                    Maybe_exhausted max_from_n, new_is)
      end
    | Bottom_val (kars_pos, maybe_found_stop, is, p) ->
      fpf "Bottom_val, kars_pos: %a, p: %a" pp_opt_int_pair kars_pos Int.pretty p;
      (* reached invalid value, either completely uninit/dangling,
         or end of base, depending on the value of [is] *)
      begin
        (* if kars_pos = None, then no valid value may be returned => sure error *)
        match kars_pos with
        | None -> Never_ok (IS.ensure_an_error is)
        | Some (lz, rz) ->
          (* both valid and invalid values exist => return alarms + valid values *)
          let min = I.(lz - offset) in
          let max = I.(rz - offset) in
          let abs_offs = Ival.inject_range (Some lz) (Some rz) in
          let char_fs = FS.Top in
          Maybe_ok (Ival.inject_range (Some min) (Some max), abs_offs,
                    char_fs, maybe_found_stop, Non_exhausted, IS.may_indet is)
      end

  (* joins two accumulators *)
  let join_acc acc r =
    match acc, r with
    | Maybe_ok (bounds1, abs_offs1, char_fs1, maybe_found_stop1, es1, is1),
      Maybe_ok (bounds2, abs_offs2, char_fs2, maybe_found_stop2, es2, is2) ->
      Maybe_ok (Ival.join bounds1 bounds2, Ival.join abs_offs1 abs_offs2,
                FS.join char_fs1 char_fs2, maybe_found_stop1 || maybe_found_stop2,
                join_exh es1 es2, IS.join is1 is2)
    | Maybe_ok (bounds, abs_offs, char_fs, maybe_found_stop, es, is1), Never_ok (is2)
    | Never_ok (is2), Maybe_ok (bounds, abs_offs, char_fs, maybe_found_stop, es, is1) ->
      fpf "join_by_offs: combining Never_ok and Maybe_ok";
      (* discard invalid one, but add its errors as a possibility *)
      Maybe_ok (bounds, abs_offs, char_fs, maybe_found_stop, es,
                IS.ensure_an_error (IS.join is1 is2))
    | Never_ok (is1), Never_ok (is2) -> Never_ok (IS.join is1 is2)

  (* Wrapper for [search] which prepares the [max_char_to_look]
     parameter and joins the result with the accumulator [acc].
     Needed by strnlen, since [last_char_to_look] may be imprecise due to the
     fact that max(n_len) may be unbounded. *)
  let search_and_acc charcharmap offset ?n_len last_char_to_look acc =
    let actual_last_char = match n_len with
      | None -> (* no change *) last_char_to_look
      | Some ival ->
        begin
          match Ival.max_int ival with
          | None -> (* no change *) last_char_to_look
          | Some max_n -> I.(min last_char_to_look (pred (offset + max_n)))
        end
    in
    fpf "search_and_acc (offset: %a), n_len: {%a}, last_char_to_look: %a, \
         adjusted last_char_to_look: %a" Int.pretty offset
      (Pretty_utils.pp_opt Ival.pretty) n_len Int.pretty last_char_to_look
      Int.pretty actual_last_char;
    let res = search charcharmap offset n_len actual_last_char in
    let res' = join_acc acc res in
    fpf "search_and_acc will return: %a" pp_str_res_t res';
    res'

end

(* To efficiently perform a search in an interval of offsets,
   we proceed from right to left, starting at the last offset to be searched. *)
module Search_ranges = struct

  (* Accumulator for the iterator of [search].
     Information about previous/best bounds is necessary due to the possibility
     of "holes" between ranges. For instance, consider the following
     charcharmap, where T/F/U stands for True/False/Unknown (Must/None/Maybe):

      F U F T U F T
     |_|_|_|_|_|_|_|
      1 2 3 4 5 6 7

     Suppose we search among all offsets in that interval, starting from the right:

     - offset 7: Must,  therefore {min=0,max=0}
     - offset 6: None,  therefore {min=0,max=1}
     - offset 5: Maybe, therefore {min=0,max=2}
     - offset 4: Must,  therefore {min=0,max=2}

     When we arrive at offset 3 (None), we must know (1) whether we are part
     of an unbroken sequence of None/Maybe (which would mean that [max] should
     be incremented), and also (2) what is the length of the *current* sequence
     of unbroken None/Maybe. Without both these numbers, we cannot know if
     the current value for [max] should increase (because we are adding yet
     another char range to it) or should stay the same (if we have started a
     new sequence which is not yet larger than the largest one previously
     found).

     Using [prev_min/prev_max] in complement to [best_min/best_max] gives us
     the required information to correctly and precisely compute the
     result of the function. *)
  type range_acc_t = {
    prev_min: Int.t option; (* minimum bound for previous range *)
    best_min: Int.t option; (* minimum bound among all previous ranges *)
    prev_max: Int.t option; (* maximum bound for previous range *)
    best_max: Int.t option; (* maximum bound among all previous ranges *)
    maybe_not_found: bool; (* whether there is a possible "not found" solution *)
    is: IS.t; (* accumulated init_status *)
    prev_es: exhausted_status;
    best_es: exhausted_status;
  }

  let pp_acc fmt acc =
    Format.fprintf fmt "@[{ranges_acc:prev_min=%a,best_min=%a,prev_max=%a,\
                        best_max=%a,init=%a,best_es=%a,prev_es=%a}@]"
      (Pretty_utils.pp_opt Int.pretty) acc.prev_min
      (Pretty_utils.pp_opt Int.pretty) acc.best_min
      (Pretty_utils.pp_opt Int.pretty) acc.prev_max
      (Pretty_utils.pp_opt Int.pretty) acc.best_max
      IS.pretty acc.is pp_exh_st acc.prev_es pp_exh_st acc.best_es

  let make_acc prev_min best_min prev_max best_max maybe_not_found is prev_es best_es =
    { prev_min; best_min; prev_max; best_max;
      maybe_not_found; is; prev_es; best_es }

  let indet_acc is =
    make_acc None None None None false (IS.ensure_an_error is)
      Non_exhausted Non_exhausted

  let update_acc acc prev_min best_min prev_max best_max maybe_not_found is prev_es best_es =
    { prev_min; best_min; prev_max; best_max; maybe_not_found;
      is = IS.join acc.is is; prev_es; best_es }

  (* initializes the accumulator used by the search by ranges *)
  let search_init charcharmap ?n_len last_char_to_look offset_end : range_acc_t =
    match
      let res =
        Search_single_offset.search charcharmap offset_end n_len last_char_to_look
      in
      fpf "search (single offset) returned: %a" pp_str_res_t res;
      res
    with
    | Never_ok (is) -> indet_acc is
    | Maybe_ok (bounds, _abs_offs, char_fs, maybe_found_stop, es, is) ->
      (* TODO: for extra precision, compute abs_offs for range and use it *)
      begin
        match char_fs with
        | FS.Bottom -> indet_acc is
        | FS.True ->
          let omin, omax = Ival.min_and_max bounds in
          let min, max = Extlib.the omin, Extlib.the omax in
          (* the string to the right surely starts with a zero *)
          let init_min = Some min in
          let init_max = Some max in (* first next char guaranteed to be '\0' *)
          make_acc init_min init_min init_max init_max maybe_found_stop is es es
        | FS.Top ->
          let omin, omax = Ival.min_and_max bounds in
          let min, max = Extlib.the omin, Extlib.the omax in
          let init_min = Some min in
          let init_max = Some max in (* first next char may not be a '\0' *)
          make_acc init_min init_min init_max init_max maybe_found_stop is es es
        | FS.False ->
          (* can only happen with strnlen or when only
             a stopping character has been found *)
          assert (n_len <> None || maybe_found_stop);
          fpf "NOT small_set: Non";
          make_acc None None None None maybe_found_stop is es es
      end

  (* Performs right-to-left traversal of the Charcharmap representing the string,
     accumulating resulting bounds and error messages along the way. *)
  let search_rtl range_start range_end bs ?n_len acc =
    let range_len = Int.length range_start range_end in
    fpf "@[search_rtl: range: %a - %a (len %a), bs: %a, acc: %a@]"
      Int.pretty range_start Int.pretty range_end Int.pretty range_len
      Str_datatype.pretty bs pp_acc acc;
    let maybe_found_stop = match bs.BS.stop_st with
      | FS.True | FS.Top -> true
      | FS.False | FS.Bottom -> false
    in
    match bs.BS.search_st with
    | FS.Bottom ->
      assert (bs.BS.stop_st = FS.Bottom);
      (*TODO: for strn*, distinguish between "sure" Invalid and "maybe" Invalid*)
      fpf "got v = Invalid, setting alarm, will propagate best_es: %a"
        pp_exh_st acc.best_es;
      { prev_min = None; best_min = acc.best_min;
        prev_max = None; best_max = acc.best_max;
        maybe_not_found = acc.maybe_not_found || maybe_found_stop;
        is = IS.may_indet bs.BS.init_st;
        prev_es = Non_exhausted; best_es = acc.best_es }
    | FS.True ->
      fpf "got v = Must, will propagate best_es: %a" pp_exh_st acc.best_es;
      let new_min = Some Int.zero in
      let new_max = Some Int.zero in
      let new_best_min = new_min in
      let new_best_max =
        match acc.best_max with
        | None -> new_max
        | _ -> acc.best_max
      in
      let new_es = Non_exhausted (* override previous exhausted_status *) in
      let best_es = acc.best_es in
      (* searched character certainly found, so only propagates maybe_not_found *)
      let maybe_not_found = acc.maybe_not_found in
      update_acc acc new_min new_best_min new_max new_best_max
        maybe_not_found bs.BS.init_st new_es best_es
    | FS.Top ->
      let new_min = Some Int.zero in
      let new_max_val =
        match acc.prev_max with
        | None -> (* last valid characters before Invalid =>
                     final character in the range assumed to be '0' =>
                     exclude it from maximum length *)
          Int.pred range_len
        | Some prev_max -> Int.add range_len prev_max
      in
      let new_max = Some new_max_val in
      let new_best_min = new_min in
      let new_best_max =
        match acc.best_max with
        | None -> new_max
        | Some old_max_val -> Some (I.max old_max_val new_max_val)
      in
      let new_es = concatenate_exh acc.prev_es range_len n_len in
      let best_es = join_exh acc.best_es new_es in
      (* maintain previous exhausted status if Maybe *)
      fpf "got v = Maybe, best_es: %a" pp_exh_st best_es;
      (* searched character possibly not found found, so join maybe_found_stop *)
      let maybe_not_found = maybe_found_stop || acc.maybe_not_found in
      update_acc acc new_min new_best_min new_max new_best_max
        maybe_not_found bs.BS.init_st new_es best_es
    | FS.False ->
      let new_min = Extlib.opt_map
          (fun prev_min -> Int.add prev_min range_len) acc.prev_min
      in
      let new_max_val =
        match acc.prev_max with
        | None -> Int.zero (* arbitrary value, never used*)
        | Some prev_max_val -> Int.add range_len prev_max_val
      in
      let new_max = Some new_max_val in
      let new_best_min = acc.best_min in
      let new_best_max =
        match acc.best_max with
        | None -> None
        | Some old_max_val -> Some (I.max old_max_val new_max_val)
      in
      let new_es = concatenate_exh acc.prev_es range_len n_len in
      let best_es = join_exh acc.best_es new_es in
      fpf "got v = Non, new_es = %a, new best_es: %a"
        pp_exh_st new_es pp_exh_st best_es;
      (* searched character not found found, so join maybe_found_stop *)
      let maybe_not_found = maybe_found_stop || acc.maybe_not_found in
      update_acc acc new_min new_best_min new_max new_best_max
        maybe_not_found bs.BS.init_st new_es best_es

  (* Used by strnlen to adjust the returned bounds, according to [n_len].
     Necessary because the simultaneous computation for several offsets/values of
     [n_len] may result in a bound that is less precise than the initial value of
     [n_len]. This function then adjusts both max and min bounds to avoid losing
     precision. *)
  let adjust_bounds min_bounds max_bounds n_len =
    try
      match n_len with
      | None -> (* no change *) (min_bounds, max_bounds)
      | Some ival ->
        begin
          let new_max = match Ival.max_int ival, max_bounds with
            | None, None -> (* nothing to do: raise Exit to return simply None *)
              raise Exit
            | None, Some max_b -> (* no change *) max_b
            | Some max_n, None -> (* replace None with new_max? *) I.pred max_n
            | Some max_n, Some max_b ->
              fpf "adjust_bounds: max_b: %a, max_n: %a"
                Int.pretty max_b Int.pretty max_n;
              I.(min max_b (I.pred max_n))
          in
          fpf "adjust_bounds: new_max = %a" Int.pretty new_max;
          match min_bounds with
          | None -> (None, Some new_max)
          | Some old_min ->
            (* adjust min to ensure it is <= max *)
            Some I.(min old_min new_max), Some new_max
        end
    with
    | Exit ->
      min_bounds, max_bounds

  let search_ptr_imprecise ~cwidth charcharmap base offset_ival n_len last_char_to_look =
    (* [max_valid_offset_chars] is the maximum possibly valid offset for
       the base, which is a better upper bound than MAX_INT. *)
    let max_valid_offset_chars =
      match Base.valid_range (Base.validity base) with
      | Base.Invalid_range -> (* should not happen... *) Int.zero
      | Base.Valid_range opt_itv -> match opt_itv with
        | None -> Int.(div (Bit_utils.max_byte_address ()) (cwidth_to_bytes cwidth))
        | Some (_, mx_bits) ->
          Int.(div mx_bits cwidth) (* possible rounding towards zero *)
    in
    (* TODO: extra precision can be obtained by splitting cases *)
    match Ival.max_int offset_ival with
    | None ->
      let max_res = match n_len with
        | None -> max_valid_offset_chars
        | Some len -> Extlib.opt_conv max_valid_offset_chars (Ival.max_int len)
      in
      let abs_offs = Ival.inject_range (Some I.zero) (Some max_res) in
      Maybe_ok (abs_offs, abs_offs, FS.top, true,
                Maybe_exhausted max_valid_offset_chars, IS.top)
    | Some max_offset_ival ->
      (* pre-computed safe upper bound in case it will be used *)
      let max_unexplored_offset = I.pred max_offset_ival in
      (* cannot use the maximum length of [n_len] as upper bound,
         because it is a relative offset *)
      let max_res = Int.min max_unexplored_offset max_valid_offset_chars in
      let init =
        Maybe_ok (Ival.bottom, Ival.bottom, FS.False, false, Non_exhausted, IS.bottom)
      in
      match Search_single_offset.search_and_acc
              charcharmap max_offset_ival ?n_len last_char_to_look init
      with
      | Never_ok _ as res ->
        if I.(max_offset_ival > zero) then
          (* there are possible solutions with lower offsets *)
          let abs_offs = Ival.inject_range (Some I.zero) (Some max_res) in
          Maybe_ok (abs_offs, abs_offs, FS.top, true, Maybe_exhausted max_res, IS.top)
        else (* there is definitely no other possible solution *)
          res
      | Maybe_ok (_bounds, abs_offs, _char_fs, _maybe_found_stop, _es, _is)
        as res when Ival.is_bottom abs_offs ->
        if I.(max_offset_ival > zero) then
          (* there are possible solutions with lower offsets *)
          let new_abs_offs = Ival.inject_range (Some I.zero) (Some max_res) in
          Maybe_ok (new_abs_offs, new_abs_offs, FS.top, true, Maybe_exhausted max_res, IS.top)
        else (* there is definitely no other possible solution *)
          res
      | Maybe_ok (_bounds, abs_offs, _char_fs, _maybe_found_stop, _es, _is) ->
        assert (not (Ival.is_bottom abs_offs));
        let max_res = Extlib.opt_conv max_valid_offset_chars (Ival.max_int abs_offs) in
        let approx_abs_offs = Ival.inject_range (Some I.zero) (Some max_res) in
        Maybe_ok (approx_abs_offs, approx_abs_offs, FS.top, true, Maybe_exhausted max_res, IS.top)

  (* [search charcharmap offset_ival offset_start offset_end n_len last_char_to_look]
     searches for a character in [charcharmap], for all offsets in [offset_ival]
     and up to all lengths in [n_len].
     [offset_start], [offset_end] and [last_char_to_look] are optimizations to
     avoid searching the entire offsetmap. *)
  (* last_char_to_look is [base_len+1] for strlen,
     or [max_offset + max_n] for strnlen.
     For strnlen, max_char_to_look may be adjusted by Search_single.search_and_acc
     to a more precise value. *)
  let search ~cwidth charcharmap ~ret_rel_offs base offset_ival offset_start offset_end ?n_len last_char_to_look =
    fpf "@[by_offset_ival: offset_ival: %a, offset_start: %a, offset_end: %a, \
         last_char_to_look: %a@]" Ival.pretty offset_ival Int.pretty offset_start
      Int.pretty offset_end Int.pretty last_char_to_look;
    let res =
      match offset_ival with
      | Ival.Set a ->
        (* more precise but less efficient version *)
        let init_acc =
          Maybe_ok (Ival.bottom, Ival.bottom, FS.False, false, Non_exhausted, IS.bottom)
        in
        Array.fold_left (fun acc offset ->
            (* for each given offset, adjust [last_char_to_look] if strnlen *)
            let res = Search_single_offset.search_and_acc
              charcharmap offset ?n_len last_char_to_look acc
            in
            fpf "search with small set, cur offset: %a, res: %a "
              Int.pretty offset pp_str_res_t res;
            res
          ) init_acc a, Not_imprecise
      | _ -> (* less precise but more efficient version *)
        (* str functions returning pointers are currently imprecise for ranges *)
        if not ret_rel_offs then
          search_ptr_imprecise ~cwidth charcharmap base offset_ival n_len last_char_to_look, Imprecise
        else begin
          fpf "by_offset_ival: not a small set!";
          let init_acc =
            search_init charcharmap ?n_len last_char_to_look offset_end
          in
          fpf "search_init returned init_acc = %a" pp_acc init_acc;
          let validity_alarm, valid_itv =
            Tr_offset.trim_by_validity
              Ival.(mul (inject_singleton cwidth) offset_ival)
              cwidth (*sizeof(char)*) (Base.validity base)
          in
          fpf "trim_by_validity (%a, %a) returned = %b, %a"
            Int.pretty offset_start Int.pretty offset_end
            validity_alarm Tr_offset.pretty valid_itv;
          match valid_itv with
          | Tr_offset.Invalid ->  (* no valid interval *)
            let is = { init_acc.is with IS.maybe_indet = true } in
            Never_ok is, Not_imprecise
          | _ ->
            let res_acc =
              Charcharmap.fold_itv ~direction:`RTL ~entire:false
                (fun (range_start, range_end) bs acc ->
                   search_rtl range_start range_end bs ?n_len acc
                ) (offset_start, (I.pred offset_end)) charcharmap init_acc
            in
            fpf "res_acc = %a" pp_acc res_acc;
            match res_acc.best_min, res_acc.best_max with
            | Some _, Some _ ->
              (* for strnlen, adjust bounds according to the [n] argument *)
              let (adj_min, adj_max) =
                adjust_bounds res_acc.best_min res_acc.best_max n_len
              in
              let maybe_indet = res_acc.is.IS.maybe_indet || validity_alarm in
              Maybe_ok (Ival.inject_range adj_min adj_max,
                        (*abs_offs not used by caller*)Ival.top,
                        (*fs not used by caller*)FS.Top, res_acc.maybe_not_found,
                        res_acc.best_es, { res_acc.is with IS.maybe_indet }),
              Not_imprecise
            | _, _ ->
              Never_ok (res_acc.is), Not_imprecise
        end
    in
    res

end

exception Top_res of Problem.t

(* [compute_maybe_invalid] is an optional triple:
   - [None]: that a definitive error has been found, so don't bother;
   - [Some (base_max_sure_char, base_end_char, abs_offs)]:
     compute if there may have been an access to offsets past the
     validity of their bases. *)
let compute_problems imprecise is compute_maybe_invalid =
  let acc_probs = Problems.empty in
  let acc_probs = match imprecise with
    | Imprecise ->
      Problems.add
        (Problem.Misc "range too large and/or imprecise, approximating")
        acc_probs
    | Not_imprecise -> acc_probs
  in
  let acc_probs = if is.IS.maybe_uninit then
      Problems.add (Problem.Init(false)) acc_probs
    else acc_probs
  in
  let acc_probs = if is.IS.maybe_esc then
      Problems.add (Problem.Esc(false)) acc_probs
    else acc_probs
  in
  let acc_probs = if is.IS.maybe_indet then
      Problems.add (Problem.Indet(false)) acc_probs
    else acc_probs
  in
  let acc_probs =
    match compute_maybe_invalid with
    | None -> (* do not compute *) acc_probs
    | Some (base_max_sure_char, base_end_char, abs_offs) ->
      if Int.lt base_max_sure_char base_end_char &&
         not (Ival.is_bottom abs_offs) then
        match Ival.max_int abs_offs with
        | None -> (* unbounded max: other warnings have already been
                     emitted, so omit this one *) acc_probs
        | Some max_char_to_look ->
          if Int.(gt max_char_to_look base_max_sure_char) then
            Problems.add Problem.Maybe_invalid acc_probs else acc_probs
      else acc_probs
  in
  acc_probs

(* Searches base [base+offset_arg], for up to [n_len] characters. *)
let search_by_base ~cwidth bs_of_vu_f ~ret_rel_offs base offset_arg ?n_len state :
  Ival.t * Ival.t * FS.t * bool * exhausted_status * Problems.t =
  fpf "base: %a (validity: %a)" Base.pretty base Base.pretty_validity
    (Base.validity base);
  let offsetmap = Cvalue.Model.find_base_or_default base state in
  match offsetmap with
  | `Bottom -> (* possibly invalid base *)
    (Ival.bottom, Ival.bottom, FS.Bottom, false, Non_exhausted,
     Problems.singleton (Problem.Base base))
  | `Top -> assert false
  | `Value offsetmap ->
    let base_max_sure_bit, base_end_bit = match Base.validity base with
      | Base.Empty -> Int.minus_one, Int.minus_one
      | Base.Known (_, max_v) -> max_v, max_v
      | Base.Unknown (min_v, None, max_v) -> min_v, max_v
      | Base.Unknown (_, Some k, max_v) -> Int.succ k, max_v
      | Base.Variable var_valid -> var_valid.Base.min_alloc, var_valid.Base.max_alloc
      | Base.Invalid -> assert false
    in
    let base_end_char = chars_of_bits ~cwidth ~inexact:true base_end_bit (*truncated*) in
    (* base_max_sure_char is only used to generate an alarm in case a possibly
       invalid location may be accessed during search *)
    let base_max_sure_char = chars_of_bits ~cwidth ~inexact:true base_max_sure_bit in
    let offset_start = Extlib.opt_conv Int.zero (Ival.min_int offset_arg) in
    let offset_start_bit = bits_of_chars ~cwidth offset_start in
    let offset_end = Extlib.opt_conv base_end_char (Ival.max_int offset_arg) in
    let max_bit_to_look =
      match n_len with
      | None -> base_end_bit
      | Some n ->
        begin
          match Ival.max_int n with
          | Some max_n ->
            (* compute the last char that is possibly examined; if [max_n] is 0,
               the last looked char is -1 (no char is examined) *)
            if I.(max_n = zero) then I.minus_one
            else
              (* look at chars between [offset_end] and [offset_end+max_n-1] *)
              let max_char_to_look = I.(pred (offset_end + max_n)) in
              fpf "max_char_to_look (before max end_base): \
                   %a (offset_end: %a, max_n: %a)" Int.pretty max_char_to_look
                Int.pretty offset_end Int.pretty max_n;
              let max_char_to_look = Int.min max_char_to_look base_end_char in
              fpf "max_char_to_look (after max end_base): %a"
                Int.pretty max_char_to_look;
              (* for each char, bits 0 to (cwidth-1) are examined *)
              I.(bits_of_chars ~cwidth max_char_to_look + (pred cwidth))
          | None -> (* fallback to base size *) base_end_bit
        end
    in
    fpf "max_bit_to_look: %a" Int.pretty max_bit_to_look;
    let max_char_to_look = chars_of_bits ~cwidth ~inexact:true max_bit_to_look in
    (* adjust max_char_to_look for strlen() if needed
       (may look past the end of the base) *)
    let max_char_to_look =
      if n_len = None then I.succ max_char_to_look else max_char_to_look
    in
    fpf "max_char_to_look (adjusted): %a" Int.pretty max_char_to_look;
    (* convert to str*-specific optimized bitwise offsetmap *)
    let charcharmap =
      make_charcharmap ~cwidth bs_of_vu_f base offsetmap offset_start_bit
        max_bit_to_look base_end_bit
    in
    fpf "charcharmap: %a" Charcharmap.pretty charcharmap;
    let res, imprecise =
      Search_ranges.search ~cwidth charcharmap ~ret_rel_offs base offset_arg
        offset_start offset_end ?n_len max_char_to_look
    in
    fpf "by_offset_ival returned: %a" pp_str_res_t res;
    match res with
    | Never_ok (is) ->
      let problems = compute_problems imprecise is None in
      (Ival.bottom, Ival.bottom, FS.Bottom, false, Non_exhausted, problems)
    | Maybe_ok (bounds, abs_offs, char_fs, maybe_found_stop, es, is) ->
      (* special case: vals is bottom, despite "Maybe_ok" =>
         due to initialization (should only happen with an empty base) *)
      if Ival.is_bottom bounds && es = Non_exhausted then
        (Ival.bottom, Ival.bottom, FS.Bottom, maybe_found_stop, es,
         Problems.singleton (Problem.Indet(true)))
      else
        let problems =
          compute_problems imprecise is
            (Some (base_max_sure_char, base_end_char, abs_offs))
        in
        (bounds, abs_offs, char_fs, maybe_found_stop, es, problems)

(* Searches the pointer(s) [base+offs] for a given char
   (according to [bs_of_vu_f]), possibly up to the length given by [n_ival].
   If the [n] argument of the original function contains a small set,
   then this function is called several times with a singleton value each time.
   Otherwise, it is called once with a non-singleton interval.
   [acc_res] and [acc_probs] contain the accumulated result
   and list of warnings. *)
let search_by_base_wrapper ~cwidth bs_of_vu_f ~ret_rel_offs state base offs ?n_ival ~include_exh () :
  Base_res.t =
  fpf "fold base(%a, offset %a)"
    Base.pretty base Ival.pretty offs;
  let (vals, abs_offs, char_fs, maybe_not_found, es, problems) =
    search_by_base ~cwidth ~ret_rel_offs bs_of_vu_f base offs state ?n_len:n_ival
  in
  let vals' = if include_exh && ret_rel_offs then
      match es with
      | Non_exhausted -> vals
      | Maybe_exhausted i_exh ->
        fpf "maybe exhausted, adding %a to vals (before filter): %a"
          Int.pretty i_exh Ival.pretty vals;
        let new_max =
          match n_ival with
          | Some n' ->
            if not (Ival.is_bottom n') then
              let max_n = Extlib.the (snd (Ival.min_and_max n')) in
              I.(min i_exh max_n)
            else i_exh
          | None -> i_exh
        in
        let new_vals = Ival.join vals (Ival.inject_singleton new_max) in
        fpf "filtered Maybe_exhausted: new_max: %a, new_vals: %a"
          Int.pretty new_max Ival.pretty new_vals;
        new_vals
    else vals
  in
  let base_res = {BR.vals = vals'; abs_offs; char_fs; maybe_not_found; es; problems} in
  fpf "@[base_res: %a, n_ival: %a@]" BR.pretty base_res
    (Pretty_utils.pp_opt Ival.pretty) n_ival;
  if not (Ival.is_bottom base_res.BR.vals) then
    begin
      match Ival.max_int base_res.BR.vals,
            Extlib.opt_bind Ival.max_int n_ival with
      | Some max_res, Some max_n ->
        (* check if the returned bounds are not larger than max_n, which should
           never happen for strnlen (but may happen for memchr). *)
        if I.(max_res > max_n) && ret_rel_offs then
          let _ = Format.printf "ASSERTION FAILURE: max_res(%a) > max_n(%a)"
              Int.pretty max_res Int.pretty max_n in
          assert false
      | _, _ -> ()
    end;
  fpf "search_by_base_wrapper will return: %a" BR.pretty base_res;
  base_res

(* searches for a character (according to function [bs_of_v_f]) inside string
   [str], in a given [state]. Returns a mapping from bases to [Base_res.t]. *)
(* Generic search function used by several built-ins.
   [name] is the built-in name (used for error messages),
   [n_ival] is used for built-ins having a length argument.
   May raise [Top_res]. *)
let search_char_n ~cwidth bs_of_vu_f ~ret_rel_offs name state ?n ~include_exh str : bm_res_t =
  try
    let str_map =
      match str with
      | Location_Bytes.Top(_, _) ->
        raise (Top_res (Problem.Misc "string argument is too imprecise, \
                                      cannot compute a useful result."))
      | Location_Bytes.Map m -> m
    in
    let offs_map = basemap_of_locmap str_map in
    let search_f = search_by_base_wrapper ~cwidth bs_of_vu_f state in
    let bm_res =
      match n with
      | None ->
        BaseMap.mapi
          (fun base offs ->
             let norm_offs = Ival.(scale_div ~pos:false (cwidth_to_bytes cwidth) offs) in
             search_f ~ret_rel_offs base norm_offs ~include_exh:false ()) offs_map
      | Some n' ->
        let n_ival_all = Cvalue.V.project_ival n' in
        match n_ival_all with
        | Ival.Set n_vals ->
          (* small set: compute a precise result for each value and join then *)
          BaseMap.mapi (fun base offs ->
              let norm_offs = Ival.(scale_div ~pos:false (cwidth_to_bytes cwidth) offs) in
              Array.fold_left (fun acc_br cur_n ->
                  let cur_n = Ival.inject_singleton cur_n in
                  let base_res =
                    search_f ~ret_rel_offs base norm_offs ~n_ival:cur_n ~include_exh ()
                  in
                  Base_res.join acc_br base_res
                ) Base_res.bottom n_vals
            ) offs_map
        | Ival.Top _ ->
          BaseMap.mapi (fun base offs ->
              let norm_offs = Ival.(scale_div ~pos:false (cwidth_to_bytes cwidth) offs) in
              search_f ~ret_rel_offs base norm_offs ~n_ival:n_ival_all ~include_exh ()
            ) offs_map
        | Ival.Float _ -> (*should not happen*)
          Value_parameters.error
            "float (%a) value in str" Ival.pretty n_ival_all;
          raise Db.Value.Aborted
    in
    bm_res
  with
  | Cvalue.V.Not_based_on_null (* from project_ival on argument [n] *) ->
    raise (Top_res (Problem.Misc
                      ("assert(no address in second argument of " ^ name ^ ")")))
  | Abstract_interp.Error_Top ->
    raise (Top_res (Problem.Misc "Ival.Error_Top"))

(* Computes an offset from a list of pairs (base, offset).
   Used by strlen and similar built-ins.
   Returns a [Cvalue.V.t]. *)
let offset_of_base_res_f (bm : bm_res_t) =
  let computed_offset =
    BaseMap.fold (fun _base base_res acc ->
        Ival.join acc base_res.BR.vals) bm Ival.bottom
  in
  fpf "computed_offset: %a" Ival.pretty computed_offset;
  Cvalue.V.inject_ival computed_offset

type exhaustion_status =
  | No_exhaustion
  | Maybe_exhaustion
  | Sure_exhaustion

(* Computes a pointer from a list of pairs (base, offset).
   Used by memchr and similar built-ins.
   [n] is used by [memchr] (and similar built-ins) to know if, in
   case of length exhaustion, NULL should be added to the list of possible
   results. In case of definitive exhaustion, then NULL becomes the only
   possible result.
   Returns a [Cvalue.V.t]. *)
let pointer_of_base_res_f ?n (bm : bm_res_t) =
  fpf "pointer_of_base_res_f (n? %a)" (Pretty_utils.pp_opt Cvalue.V.pretty) n;
  let maybe_not_found = ref false in
  let may_return_null = ref false in
  (* possible exhaustion of the length argument [n] is detected by
     checking if the offset may contain any value in [n];
     definitive exhaustion is detected when the minimum offset is
     equal to the maximum [n]. *)
  let exhaustion_status =
    match n with
    | None -> fun _offs -> No_exhaustion (* no length => never exhausted *)
    | Some v ->
      let n_ival = Cvalue.V.project_ival v in
      fun offs ->
        if Ival.is_bottom offs then (* filtered by filter_offs *) Sure_exhaustion
        else
          let min_offs = I.the_min offs in
          let max_n = match Ival.min_and_max n_ival with
            | _, None -> assert false
            | _, Some max -> max
          in
          fpf "exhaustion (min_offs: %a, max_n: %a) ? %s"
            Int.pretty min_offs Int.pretty max_n
            (if I.(min_offs >= max_n) then "Sure" else
             if Ival.intersects offs n_ival then "Maybe" else "No");
          if I.(min_offs >= max_n) then Sure_exhaustion
          else if Ival.intersects offs n_ival then Maybe_exhaustion
          else No_exhaustion
  in
  let res =
    BaseMap.fold (fun base base_res acc ->
        if base_res.BR.maybe_not_found then maybe_not_found := true;
        if base_res.BR.es <> Non_exhausted (*BR.has_null_result*) then
          (fpf "base possibly exhausted (%a), may return null"
             pp_exh_st base_res.BR.es;
          may_return_null := true);
        let offset_ival = base_res.BR.vals in
        fpf "BaseMap.fold: iterating base %a, offset %a"
          Base.pretty base Ival.pretty offset_ival;
        if Ival.is_bottom offset_ival then acc
        else
          match exhaustion_status offset_ival with
          | Sure_exhaustion ->
            fpf "SURE exhaustion for base %a, offset_ival %a; NOT adding abs_offs: %a"
              Base.pretty base Ival.pretty offset_ival Ival.pretty base_res.BR.abs_offs;
            may_return_null := true;
            acc (* do not add this base *)
          | Maybe_exhaustion ->
            fpf "MAYBE exhaustion for base %a, offset_ival %a; adding abs_offs: %a"
              Base.pretty base Ival.pretty offset_ival Ival.pretty base_res.BR.abs_offs;
            may_return_null := true;
            Cvalue.V.join acc (Cvalue.V.inject base base_res.BR.abs_offs)
          | No_exhaustion ->
            fpf "NO exhaustion for base %a, offset_ival %a; adding abs_offs: %a"
              Base.pretty base Ival.pretty offset_ival Ival.pretty base_res.BR.abs_offs;
            Cvalue.V.join acc (Cvalue.V.inject base base_res.BR.abs_offs)
      ) bm Cvalue.V.bottom
  in
  if !may_return_null || !maybe_not_found then
    let _ = fpf "joining null base" in
    Cvalue.V.join res (Cvalue.V.inject Base.null Ival.zero)
  else res

type str_builtin_type =
  | Search_zero_stop_zero
  | Search_char_stop_char
  | Search_char_stop_zero

module String_alarms = Datatype.Triple_with_collections(Alarms)(Datatype.String)(Datatype.String)
    (struct let module_name = "Builtins_string.String_alarms.t" end)

exception Invalid_nb_of_args of int

type expterm =
  | Exp of Cil_types.exp
  | Term of Cil_types.term

(* Prints the expression/term [et] if [print_val] is false,
   otherwise prints the abstract value [v]. *)
let pretty_arg ~print_val fmt (et, v) =
  if print_val then
    let typ = match et with
      | Exp e -> Some (Cil.typeOf e)
      | Term t -> match t.Cil_types.term_type with
        | Cil_types.Ctype ty -> Some ty
        | _ -> assert false
    in
    Cvalue.V.pretty_typ typ fmt v
  else
    match et with
    | Exp e -> Printer.pp_exp fmt e
    | Term t -> Printer.pp_term fmt t

let pretty_args ~print_val fmt args =
  Pretty_utils.pp_flowlist (pretty_arg ~print_val) fmt args

type str_builtin_sig =
  Cvalue.Model.t (*state*) ->
  (expterm * Cvalue.V.t) list (*args*) ->
  Value_types.call_result * String_alarms.Set.t (*res,alarms*)

(* Wrapper for [search_char_n] which checks arguments
   (according to [has_char] and [has_n]), calls [search_char_n],
   computes the result and the alarms, and produces the output
   (according to [is_ret_pointer]). Does not emit the produced alarms. *)
let search_char_n_wrapper ~cwidth name nb_args str_builtin_type ~has_n ~is_ret_pointer state args =
  (* prepare auxiliary function *)
  let eval_op_wrapper =
    if is_ret_pointer then Eval_op.wrap_ptr else Eval_op.wrap_size_t
  in
  try
    let (et_str, str) = List.nth args 0 in
    let has_char = str_builtin_type <> Search_zero_stop_zero in
    let n =
      if has_n then
        let n_index = if has_char then 2 else 1 in
        let (_exp_n, n') = List.nth args n_index in
        Some n'
      else None
    in
    (* prepare auxiliary function *)
    let res_of_base_res_f =
      if is_ret_pointer then (pointer_of_base_res_f ?n) else offset_of_base_res_f
    in
    let value, problems =
      try
        let bs_of_vu_f =
          match str_builtin_type with
          | Search_zero_stop_zero -> zero_zero_bs_of_vu
          | Search_char_stop_char ->
            let (_, chr) = List.nth args 1 in
            char_char_bs_of_vu (Cvalue.V.project_ival chr)
          | Search_char_stop_zero ->
            let (_, chr) = List.nth args 1 in
            char_zero_bs_of_vu (Cvalue.V.project_ival chr)
        in
        let bm = search_char_n ~cwidth bs_of_vu_f ~ret_rel_offs:(not is_ret_pointer) name state ?n
            ~include_exh:(not is_ret_pointer) str
        in
        let problems = BaseMap.fold (fun _base base_res acc ->
            Problems.union acc base_res.BR.problems) bm Problems.empty
        in
        res_of_base_res_f bm, problems
      with
      | Cvalue.V.Not_based_on_null (* project_ival on chr *) ->
        Cvalue.V.top_int, Problems.singleton
          (Problem.Misc
             ("assert(no address in second argument of " ^ name ^ ")"))
      | Top_res prob ->
        Cvalue.V.top_int, Problems.singleton prob
    in
    (* In case at least one base has a valid result,
       'weaken' alarm messages ("possibly ...") *)
    let problems = if not (Cvalue.V.is_bottom value) then
        Problems.weaken problems else problems
    in
    let alarms = if Problems.is_empty problems then
        String_alarms.Set.empty
      else
        begin
          let s = Format.asprintf "@[<h>\\valid_nstring(%a,%s(%a))@]"
              (pretty_arg ~print_val:false) (et_str, str)
              name (pretty_args ~print_val:false) args
          in
          let warning_msg = Format.asprintf "@[builtin %s:@ %a@]"
              name Problems.pretty problems
          in
          String_alarms.Set.singleton (Alarms.Valid_string Cil_datatype.Exp.dummy, s, warning_msg)
        end;
    in
    let res_c_value = if Cvalue.V.is_bottom value then
        None, Cvalue.Model.bottom
      else eval_op_wrapper value, state
    in
    { Value_types.c_values = [res_c_value];
      c_clobbered = Base.SetLattice.bottom;
      c_from = None;
      c_cacheable = Value_types.Cacheable;}, alarms
  with
  | Failure _ ->
    let bt = Printexc.get_backtrace () in
    fpf "UNCAUGHT EXCEPTION:@.%s" bt;
    raise (Invalid_nb_of_args nb_args)

let emit_alarms alarms =
  String_alarms.Set.iter (fun (kind, text, warning_msg) ->
      if Builtins.emit_alarm ~kind:(Alarms.get_name kind) ~text then
        Value_util.alarm_report ~source:(fst (Cil_const.CurrentLoc.get()))
          "%s" warning_msg
    ) alarms


let args_of_actuals = List.map (fun (e,v,_) -> (Exp e, v))

(* Export the builtin as an OCaml function, and also registers it as a
   Value builtin, of name [name]. *)
let export_and_register c_name nb_args str_builtin_type ~has_n ~is_ret_pointer ~cwidth =
  let name = "Frama_C_" ^ c_name in
  let print_call actuals =
    (*reset_callstack_base ();*)
    if Value_parameters.ValShowProgress.get () then
      Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
        name (pretty_args ~print_val:true) actuals Value_util.pp_callstack;
    fpf "=-=-=-=-=-= NEW CALL TO NEW_FRAMA_C_%s (%a) =-=-=-=-=-="
      name Printer.pp_location (Cil_const.CurrentLoc.get());
  in
  let f =
    search_char_n_wrapper ~cwidth name nb_args str_builtin_type ~has_n ~is_ret_pointer
  in
  let f_builtin state actuals =
    let actuals = args_of_actuals actuals in
    print_call actuals;
    try
      let res, alarms = f state actuals in
      emit_alarms alarms;
      res
    with Invalid_nb_of_args i ->
      raise (Builtins.Invalid_nb_of_args i)
  in
  Builtins.register_builtin name ~replace:c_name f_builtin;
  (f: str_builtin_sig)

let frama_c_strlen_wrapper  =
  export_and_register "strlen" 1
    Search_zero_stop_zero ~has_n:false ~is_ret_pointer:false ~cwidth:I.eight

let frama_c_strnlen_wrapper =
  export_and_register "strnlen" 2
    Search_zero_stop_zero ~has_n:true ~is_ret_pointer:false ~cwidth:I.eight

let frama_c_rawmemchr_wrapper =
  export_and_register "rawmemchr" 2
    Search_char_stop_char ~has_n:false ~is_ret_pointer:true ~cwidth:I.eight

let frama_c_memchr_wrapper =
  export_and_register "memchr" 3
    Search_char_stop_char ~has_n:true ~is_ret_pointer:true ~cwidth:I.eight

let frama_c_strchr_wrapper =
  export_and_register "strchr" 2
    Search_char_stop_zero ~has_n:false ~is_ret_pointer:true ~cwidth:I.eight

(* because wchar_t depends on the machdep, wchar.h builtins are defined
   differently from those in string.h. *)
let frama_c_wcslen_wrapper () =
  let cwidth = I.of_int (Cil.bitsSizeOf Cil.theMachine.Cil.wcharType) in
  export_and_register "wcslen" 1
    Search_zero_stop_zero ~has_n:false ~is_ret_pointer:false ~cwidth

let () = Db.Main.extend
    (fun () ->
       let _ = frama_c_wcslen_wrapper () in
       ()
    )
