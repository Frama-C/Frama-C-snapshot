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

module M = Alarms.Map

type alarm = Alarms.t
type status = True | False | Unknown
type s = status M.t
type t = Just of s | AllBut of s

type 'a if_consistent = [ `Value of 'a | `Inconsistent ]

let string_of_predicate_status = function
  | Unknown -> "unknown"
  | True -> "valid"
  | False -> "invalid"

let pretty_status fmt v =
  Format.fprintf fmt "%s" (string_of_predicate_status v)

module Status = struct
  include Datatype.Make_with_collections
      (struct
        type t = status
        include Datatype.Serializable_undefined
        let name = "Alarmset.status"
        let reprs = [ True; False; False; Unknown ]
        let mem_project = Datatype.never_any_project
        let pretty = pretty_status
        let compare (s1:t) (s2:t) = Pervasives.compare s1 s2
        let equal (s1:t) (s2:t) = s1 = s2
        let hash (s:t) = Hashtbl.hash s
      end)

  let join x y = match x, y with
    | True, True -> True
    | False, False -> False
    | True, False | False, True
    | Unknown, _ | _, Unknown -> Unknown

  let inter x y = match x, y with
    | Unknown, _ -> `Value y
    | _, Unknown -> `Value x
    | True, True -> `Value True
    | False, False -> `Value False
    | True, False | False, True -> `Inconsistent

  exception Stop

  let join_list l =
    try
      let r =
        List.fold_left
          (fun acc e ->
             match e, acc with
             | Unknown, _ -> raise Stop
             | e, None -> Some e
             | e, Some eacc -> Some (join eacc e)
          ) None l
      in
      match r with
      | None -> True
      | Some r -> r
    with Stop -> Unknown
end

module D = Alarms.Map.Make (Status)

let pretty fmt = function
  | Just m   -> Format.fprintf fmt "Just %a" D.pretty m
  | AllBut m -> Format.fprintf fmt "AllBut %a" D.pretty m

let none = Just M.empty
let all = AllBut M.empty

let equal a b = match a, b with
  | Just m, Just n
  | AllBut m, AllBut n -> M.equal Status.equal m n
  | _, _ -> false

let get = function Just m | AllBut m -> m

let is_empty = function
  | AllBut _ -> false
  | Just m -> M.is_empty m

let singleton a = Just (M.singleton a Unknown)

let set alarm status = function
  | Just m   ->
    if status = True
    then Just (M.remove alarm m)
    else Just (M.add alarm status m)
  | AllBut m ->
    if status = Unknown
    then AllBut (M.remove alarm m)
    else AllBut (M.add alarm status m)

let add alarm = function
  | Just m   -> Just (M.add alarm Unknown m)
  | AllBut m -> AllBut (M.add alarm True m)

let default = function
  | Just _ -> True
  | AllBut _ -> Unknown

let find alarm set =
  try match set with
    | Just m
    | AllBut m -> M.find alarm m
  with Not_found -> default set

let merge merge_status s1 s2 =
  let d1 = default s1 and d2 = default s2 in
  let m1 = get s1 and m2 = get s2 in
  let closed_set = match merge_status d1 d2 with
    | True -> true
    | Unknown -> false
    | False -> assert false
  in
  let return = if closed_set
    then function True -> None | p -> Some p
    else function Unknown -> None | p -> Some p
  in
  let merge _ p1 p2 = match p1, p2 with
    | None, None       -> assert false
    | Some p, None     -> return (merge_status p d2)
    | None, Some p     -> return (merge_status d1 p)
    | Some p1, Some p2 -> return (merge_status p1 p2)
  in
  if closed_set
  then Just (M.merge merge m1 m2)
  else AllBut (M.merge merge m1 m2)

let union = merge Status.join

exception Inconsistent
let intersect status1 status2 = match Status.inter status1 status2 with
  | `Value status -> status
  | `Inconsistent -> raise Inconsistent

let inter s1 s2 =
  try `Value (merge intersect s1 s2)
  with Inconsistent -> `Inconsistent

let iter f = function
  | Just m -> M.iter f m
  | AllBut _ -> assert false

let exists test ~default = function
  | Just m -> M.exists test m || default True
  | AllBut m -> M.exists test m || default Unknown

let for_all test ~default = function
  | Just m -> M.for_all test m || default True
  | AllBut m -> M.for_all test m || default Unknown


(* --------------------------------------------------------------------------
                                 Alarms
     ------------------------------------------------------------------------ *)

open CilE

let emitter = Value_util.emitter

(* Printer that shows additional information about temporaries *)
let local_printer: Printer.extensible_printer =
  let open Cil_types in object (self)
    inherit Printer.extensible_printer () as super

    (* Temporary variables for which we want to print more information *)
    val mutable temporaries = Cil_datatype.Varinfo.Set.empty

    method! code_annotation fmt ca =
      temporaries <- Cil_datatype.Varinfo.Set.empty;
      match ca.annot_content with
      | AAssert(_, p) ->
        (* ignore the ACSL name *)
        Format.fprintf fmt "@[<v>@[assert@ %a;@]" self#predicate_node p.pred_content;
        (* print temporary variables information *)
        if not (Cil_datatype.Varinfo.Set.is_empty temporaries) then begin
          Format.fprintf fmt "@ @[(%t)@]" self#pp_temporaries
        end;
        Format.fprintf fmt "@]";
      | _ -> assert false

    method private pp_temporaries fmt =
      let pp_var fmt vi =
        Format.fprintf fmt "%s from@ @[%s@]" vi.vname (Extlib.the vi.vdescr)
      in
      Pretty_utils.pp_iter Cil_datatype.Varinfo.Set.iter
        ~pre:"" ~suf:"" ~sep:",@ " pp_var fmt temporaries

    method! logic_var fmt lvi =
      (match lvi.lv_origin with
       | None | Some { vdescr = None }-> ()
       | Some ({ vdescr = Some _ } as vi) ->
         temporaries <- Cil_datatype.Varinfo.Set.add vi temporaries
      );
      super#logic_var fmt lvi
  end

let pr_annot = local_printer#code_annotation

(* Default behaviour: print one alarm per kinstr. *)
module Alarm_key = Datatype.Pair_with_collections
  (Cil_datatype.Kinstr)(Alarms)(struct
      let module_name = "Alarm_key"
  end);;
module Alarm_cache = State_builder.Hashtbl(Alarm_key.Hashtbl)(Datatype.Unit)(struct
  let name = "Value_messages.Alarm_cache"
  let dependencies = [Db.Value.self]
  let size = 35
end)

let loc ki = match ki with
  | Cil_types.Kglobal -> (* can occur in case of obscure bugs (already happened)
                            with wacky initializers. Module Initial_state of
                            value analysis correctly positions the loc *)
    Cil.CurrentLoc.get ()
  | Cil_types.Kstmt s -> Cil_datatype.Stmt.loc s

let default_alarm_report ki alarm str =
  Alarm_cache.memo (fun (_ki,_alarm) ->
      let loc = loc ki in
      Value_util.alarm_report ~source:(fst loc) "%s" str
    ) (ki,alarm)
;;

let register_alarm ?kf ki alarm status f =
  let status = match status with
    | True -> Property_status.True
    | False -> Property_status.False_if_reachable
    | Unknown -> Property_status.Dont_know
  in
  let annot, _is_new =
    Alarms.register ~loc:(loc ki) ?kf ~status emitter ki alarm
  in
  let k =
    Format.kfprintf
      (fun _fmt -> Format.flush_str_formatter ()) Format.str_formatter
  in
  let str = f annot k Value_util.pp_callstack in
  default_alarm_report ki alarm str

type place = At of kinstr | Nowhere

let do_warn place {a_log; a_call} alarm status str =
  match place, a_log with
  | At kinstr, true ->
    register_alarm kinstr alarm status
      (fun annot k -> k "@[%s.@ %a@]%t" str pr_annot annot);
    a_call ()
  | _, _ -> a_call ()

let warn place warn_mode alarm (status:status) =
  match alarm with
  | Alarms.Pointer_comparison (_, e) ->
    let emit = match Value_parameters.WarnPointerComparison.get () with
      | "none" -> false
      | "all" -> true
      | "pointer" -> Cil.isPointerType (Cil.typeOf e)
      | _ -> assert false
    in
    if emit then
      do_warn place warn_mode.defined_logic alarm status "pointer comparison"

  | Alarms.Division_by_zero _ ->
    do_warn place warn_mode.others alarm status "division by zero"

  | Alarms.Overflow (kind, _, _, _) ->
    do_warn place warn_mode.others alarm status
      (Format.sprintf "%s"
         (match kind with
          | Alarms.Signed -> "signed overflow"
          | Alarms.Unsigned -> "unsigned overflow"
          | Alarms.Signed_downcast -> "signed downcast"
          | Alarms.Unsigned_downcast -> "unsigned downcast"
         ))

  | Alarms.Float_to_int _ ->
    do_warn place warn_mode.others alarm status
      "overflow in conversion from floating-point to integer"

  | Alarms.Invalid_shift (_, Some _) ->
    do_warn place warn_mode.others alarm status
      "invalid RHS operand for shift"

  | Alarms.Invalid_shift (_, None) ->
    do_warn place warn_mode.others alarm status
      "invalid LHS operand for left shift"

  | Alarms.Memory_access (_, access_kind)
  | Alarms.Logic_memory_access (_, access_kind) ->
    do_warn place warn_mode.others alarm status
      (Format.sprintf "out of bounds %s"
         (match access_kind with
          | Alarms.For_reading -> "read" | Alarms.For_writing -> "write"))

  | Alarms.Index_out_of_bound _ ->
    do_warn place warn_mode.others alarm status
      "accessing out of bounds index"

  | Alarms.Valid_string _ ->
    do_warn place warn_mode.defined_logic alarm status
      "may not point to a valid string"

  | Alarms.Differing_blocks _ ->
    do_warn place warn_mode.defined_logic alarm status
      "pointer subtraction"

  | Alarms.Is_nan_or_infinite (_, fkind) ->
    let sfkind = match fkind with
      | Cil_types.FFloat -> "float"
      | Cil_types.FDouble -> "double"
      | Cil_types.FLongDouble -> "long double"
    in
    do_warn place warn_mode.others alarm status
      (Format.sprintf "non-finite %s value" sfkind)

  | Alarms.Uninitialized _ ->
    do_warn place warn_mode.unspecified alarm status
      "accessing uninitialized left-value"

  | Alarms.Dangling _ ->
    do_warn place warn_mode.unspecified alarm status
      "accessing left-value that contains escaping addresses"

  | Alarms.Not_separated _ ->
    do_warn place warn_mode.others alarm status
      "undefined multiple accesses in expression"

  | Alarms.Overlap _ ->
    do_warn place warn_mode.others alarm status
      "partially overlapping lvalue assignment"

  | Alarms.Function_pointer _ ->
    do_warn place warn_mode.others alarm status
      "pointer to function with incompatible type"

let rec height_expr expr =
  match expr.enode with
  | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ -> 0
  | Lval lv | AddrOf lv | StartOf lv  -> height_lval lv + 1
  | UnOp (_,e,_) | CastE (_, e) | Info (e,_) | SizeOfE e | AlignOfE e
    -> height_expr e + 1
  | BinOp (_,e1,e2,_) -> max (height_expr e1) (height_expr e2) + 1

and height_lval (host, offset) =
  let h1 = match host with
    | Var _ -> 0
    | Mem e -> height_expr e
  in
  max h1 (height_offset offset) + 1

and height_offset = function
  | NoOffset  -> 0
  | Field (_,r) -> height_offset r + 1
  | Index (e,r) -> max (height_expr e) (height_offset r) + 1

let height_alarm = function
  | Alarms.Division_by_zero e
  | Alarms.Index_out_of_bound (e,_)
  | Alarms.Invalid_shift (e,_)
  | Alarms.Overflow (_,e,_,_)
  | Alarms.Float_to_int (e,_,_)
  | Alarms.Function_pointer e
  | Alarms.Valid_string e -> height_expr e + 1
  | Alarms.Pointer_comparison (None,e) -> height_expr e + 2
  | Alarms.Memory_access (lv,_)
  | Alarms.Dangling lv -> height_lval lv + 1
  | Alarms.Uninitialized lv -> height_lval lv
  | Alarms.Pointer_comparison (Some e1,e2) -> max (height_expr e1) (height_expr e2) + 2
  | Alarms.Differing_blocks (e1,e2) -> max (height_expr e1) (height_expr e2) + 1
  | Alarms.Not_separated (lv1,lv2)
  | Alarms.Overlap (lv1,lv2) -> max (height_lval lv1) (height_lval lv2) + 1
  | Alarms.Logic_memory_access (_,_) -> 0
  | Alarms.Is_nan_or_infinite (e,fkind) ->
    let trivial = match Cil.typeOf e with
      | TFloat (fk, _) -> fk = fkind
      | _ -> false
    in
    if trivial then height_expr e else height_expr e + 1

let cmp a1 a2 =
  Datatype.Int.compare (height_alarm (fst a1)) (height_alarm (fst a2))

let emit do_warn warn_mode = function
  | Just m ->
    let list = M.bindings m in
    let sorted_list = List.sort cmp list in
    List.iter (fun (alarm, status) ->
        warn do_warn warn_mode alarm status) sorted_list
  (* TODO: use GADT to avoid this assert false ? *)
  | AllBut  _ ->
    Value_parameters.abort ~current:true ~once:true
      "All alarms may arise: abstract state too imprecise to continue the analysis."

let notify warn_mode alarms = emit Nowhere warn_mode alarms
let emit warn_mode ki alarms = emit (At ki) warn_mode alarms


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
