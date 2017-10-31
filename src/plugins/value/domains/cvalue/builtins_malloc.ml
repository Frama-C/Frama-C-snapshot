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
open Abstract_interp
open Locations
open Cvalue

let dkey = Value_parameters.register_category "malloc"

(** {1 Dynamically allocated bases} *)

module Base_hptmap = Hptmap.Make
    (Base.Base)
    (Value_types.Callstack)
    (Hptmap.Comp_unused)
    (struct let v = [ [ ] ] end)
    (struct let l = [ Ast.self ] end)

module Dynamic_Alloc_Bases =
  State_builder.Ref
    (Base_hptmap)
    (struct
      let dependencies = [Ast.self] (* TODO: should probably depend on Value
                                       itself *)
      let name = "Value.Builtins_malloc.Dynamic_Alloc_Bases"
      let default () = Base_hptmap.empty
    end)
let () = Ast.add_monotonic_state Dynamic_Alloc_Bases.self

(** {1 Auxiliary functions} *)

(* Remove some parts of the callstack:
   - Remove the bottom of the call tree until we get to the call site
   of the call to the first malloc function. The idea is that each of
   these call site correspond to a different use of a malloc function,
   so it is interesting to keep their bases separated. *)
let call_stack_no_wrappers () =
  let stack = Value_util.call_stack () in
  assert (stack != []);
  let wrappers = Value_parameters.MallocFunctions.get() in
  let rec bottom_filter = function
    | [] -> assert false
    | [_] as stack -> stack (* Do not empty the stack completely *)
    | (kf,_)::((kf', _):: _ as rest) as stack ->
      if Datatype.String.Set.mem (Kernel_function.get_name kf) wrappers then
        if Datatype.String.Set.mem (Kernel_function.get_name kf') wrappers then
          bottom_filter rest
        else
          stack
      else
        stack
  in
  bottom_filter stack
;;

let register_malloced_base ?(stack=call_stack_no_wrappers ()) b =
  let stack_without_top = List.tl stack in
  Dynamic_Alloc_Bases.set (Base_hptmap.add b stack_without_top (Dynamic_Alloc_Bases.get ()))

let fold_dynamic_bases (f: Base.t -> Value_types.Callstack.t -> 'a -> 'a) init =
  Base_hptmap.fold f (Dynamic_Alloc_Bases.get ()) init

(* only returns true for bases allocated via alloca() *)
let is_automatically_deallocated base =
  match base with
  | Base.Allocated (_, Base.Alloca, _) -> true
  | _ -> false

(* Extracts the minimum/maximum sizes (in bytes) for malloc/realloc/calloc,
   respecting the bounds of size_t.
   Note that the value returned for maximum size corresponds to one past
   the last valid index. *)
let extract_size sizev_bytes =
  let max = Bit_utils.max_byte_size () in
  try
    let sizei_bytes = Cvalue.V.project_ival sizev_bytes in
    begin match Ival.min_and_max sizei_bytes with
      | Some smin, Some smax ->
        assert (Integer.(ge smin zero));
        smin, Integer.min smax max
      | _ -> assert false (* Cil invariant: cast to size_t *)
    end
  with V.Not_based_on_null -> (* size is a garbled mix *)
    Integer.zero, max

(* Name of the base that will be given to a malloced variable, determined
   using the callstack. *)
let base_name prefix stack =
  let stmt_line stmt = (fst (Cil_datatype.Stmt.loc stmt)).Lexing.pos_lnum in
  match stack with
    | [] -> assert false
    | [kf, Kglobal] -> (* Degenerate case *)
      Format.asprintf "__%s_%a" prefix Kernel_function.pretty kf
    | (_, Kglobal) :: _ :: _ -> assert false
    | (_, Kstmt callsite) :: qstack ->
      (* Use the whole call-stack to generate the name *)
      let rec loop_full = function
        | [_, Kglobal] -> Format.sprintf "_%s" (Kernel.MainFunction.get ())
        | (_, Kglobal) :: _ :: _ -> assert false
        | [] -> assert false (* impossible, we should have seen a Kglobal *)
        | (kf, Kstmt line)::b ->
          let line = stmt_line line in
          let node_str = Format.asprintf "_l%d__%a"
            line Kernel_function.pretty kf
          in
          (loop_full b) ^ node_str
      in
      (* Use only the name of the caller to malloc for the name *)
      let caller = function
        | [] -> assert false (* caught above *)
        | (kf, _) :: _ -> Format.asprintf "_%a" Kernel_function.pretty kf
      in
      let full_name = false in
      Format.asprintf "__%s%s_l%d"
        prefix
        (if full_name then loop_full qstack else caller qstack)
        (stmt_line callsite)
;;

type var = Weak | Strong

let create_new_var stack prefix type_base weak =
  let prefix = match weak with
    | Weak -> prefix ^ "_w"
    | Strong -> prefix
  in
  let name = Cabs2cil.fresh_global (base_name prefix stack) in
  Value_util.create_new_var name type_base

(* This function adds a "_w" information to a variable. It should be used
   when a variable becomes weak, and supposes that the variable has been
   created by one of the functions of this module. Mutating variables name
   is not a good idea in general, but we take the risk here. *)
let mutate_name_to_weak vi =
  Value_parameters.result ~dkey ~current:true ~once:false
    "@[marking variable `%s' as weak@]" vi.vname;
  try
    let prefix, remainder =
      Scanf.sscanf vi.vname "__%s@_%s" (fun s1 s2 -> (s1, s2))
    in
    let name' = Printf.sprintf "__%s_w_%s" prefix remainder in
    vi.vname <- name'
  with End_of_file | Scanf.Scan_failure _ -> ()

(* This type represents the size requested to malloc/realloc and co. *)
type typed_size = {
  min_bytes: Integer.t (* minimum size requested, in bytes *);
  max_bytes: Integer.t (* maximum size requested, in bytes *);
  elem_typ: typ (* "guessed type" for the elements of the new variable *);
  nb_elems: Integer.t option (* number of elements of size [sizeof(elem_typ)].
                                None if [min<>max] *);
}

(* Guess the intended type for the cell returned by malloc, given [sizev ==
   [size_min .. size_max] (in bytes). We look for [T *v = malloc(foo)], then
   check that [size_min] and [size_max] are multiples of [sizeof(T)].
   Note that [sizeof(T)] can be zero (e.g. an empty struct).
   If no information can be found, we use char for the base type. If the size
   cannot change later ([constant_size]), we also compute the number of
   elements that are allocated. *)
(* TODO: this is not perfect because we can have an overflow in computations
   such as [foo * t = malloc (i * sizeof(foo))] *)
let guess_intended_malloc_type stack sizev constant_size =
  let size_min, size_max = extract_size sizev in
  let nb_elems elem_size =
    if constant_size && Int.equal size_min size_max
    then Some (if Int.(equal elem_size zero) then Int.zero
               else Int.div size_min elem_size)
    else None
  in
  let mk_typed_size t =
    match Cil.unrollType t with
    | TPtr (t, _) when not (Cil.isVoidType t) ->
      let s = Int.of_int (Cil.bytesSizeOf t) in
      if Int.(equal s zero) ||
         (Int.equal (Int.rem size_min s) Int.zero &&
          Int.equal (Int.rem size_max s) Int.zero)
      then
        { min_bytes = size_min; max_bytes = size_max;
          elem_typ = t; nb_elems = nb_elems s }
      else raise Exit
    | _ -> raise Exit
  in
  try
    match snd (List.hd stack) with
    | Kstmt {skind = Instr (Call (Some lv, _, _, _))} ->
        mk_typed_size (Cil.typeOfLval lv)
    | Kstmt {skind = Instr(Local_init(vi, _, _))} -> mk_typed_size vi.vtype
    | _ -> raise Exit
  with Exit | Cil.SizeOfError _ -> (* Default, use char *)
    { min_bytes = size_min; max_bytes = size_max; elem_typ = Cil.charType;
      nb_elems = nb_elems Int.one }

(* Helper function to create the best type for a new base.  Builds an
   array type with the appropriate number of elements if needed.  When
   the number of elements cannot be determined, build an array with
   imprecise size. This is not a problem in practice, because in C you
   annot obtain the size of an allocated block, and \block_length
   handles Allocated variables through their validity. *)
let type_from_nb_elems tsize =
  let typ = tsize.elem_typ in
  match tsize.nb_elems with
  | None -> TArray (typ, None, Cil.empty_size_cache (), [])
  | Some nb ->
    if Int.equal Int.one nb
    then typ
    else
      let loc = Cil.CurrentLoc.get () in
      let esize_arr = Cil.kinteger64 ~loc nb in (* [nb] fits in size_t *)
      TArray (typ, Some esize_arr, Cil.empty_size_cache (), [])

(* Generalize a type into an array type without size. Useful for variables
   whose size is mutated. *)
let weaken_type typ =
  match Cil.unrollType typ with
  | TArray (_, None, _, _) -> typ
  | TArray (typ, Some _, _, _) | typ ->
    TArray (typ, None, Cil.empty_size_cache (), [])

(* size for which the base is certain to be valid *)
let size_sure_valid b = match Base.validity b with
  | Base.Invalid | Base.Empty | Base.Unknown (_, None, _) -> Integer.zero
  | Base.Known (_, up) | Base.Unknown (_, Some up, _)
  | Base.Variable { Base.min_alloc = up } -> Integer.succ up
;;

(* Create a new offsetmap initialized to [bottom] on the entire allocable
   range, with the first [max_alloc] bits set to [v].
   [v] must be an isotropic value. *)
let offsm_with_v v validity max_alloc =
  let size = Bottom.non_bottom (V_Offsetmap.size_from_validity validity) in
  let offsm = V_Offsetmap.create_isotropic ~size V_Or_Uninitialized.bottom in
  (* max_alloc is -1 when allocating an empty base *)
  if Int.(lt max_alloc zero) then
    (* malloc(0) => nothing to uninitialize *)
    offsm
  else (* malloc(i > 0) => uninitialize i bytes *)
    V_Offsetmap.add ~exact:true (Int.zero, max_alloc)
      (v, Int.one, Rel.zero) offsm

(* add [v] as a possible value for the bits [0..max_valid_bits] of
   [base] in [state].
   [v] must be an isotropic value. *)
let add_v v state base max_valid_bits =
  let validity = Base.validity base in
  let offsm = offsm_with_v v validity max_valid_bits in
  let new_offsm =
    try
      let cur = match Model.find_base_or_default base state with
        | `Top -> assert false (* Value never passes Top as state *)
        | `Bottom -> assert false (* implicitly checked by offsm_with_uninit *)
        | `Value m -> m
      in
      V_Offsetmap.join offsm cur
    with Not_found -> offsm
  in
  Model.add_base base new_offsm state

let add_uninitialized = add_v V_Or_Uninitialized.uninitialized
let add_zeroes = add_v (V_Or_Uninitialized.initialized Cvalue.V.singleton_zero)

let wrap_fallible_malloc ?(returns_null=Value_parameters.MallocReturnsNull.get ()) ret_base orig_state state_after_alloc =
  let ret = V.inject ret_base Ival.zero in
  let success = Eval_op.wrap_ptr ret, state_after_alloc in
  if returns_null
  then
    let failure = Eval_op.wrap_ptr Cvalue.V.singleton_zero, orig_state in
    [ success ; failure ]
  else [ success ]

let pp_validity fmt (v1, v2) =
  if Int.equal v1 v2
  then Format.fprintf fmt "0..%a" Int.pretty v1
  else Format.fprintf fmt "0..%a/%a" Int.pretty v1 Int.pretty v2


(** {1 Malloc} *)

(* Create a new variable of size [sizev] with deallocation type [deallocation],
   using [stack] to infer a type.
   Returns the new base, and its maximum validity.
   Note that [_state] is not used, but it is present to ensure a compatible
   signature with [alloc_by_stack]. *)
let alloc_abstract weak deallocation stack prefix sizev _state =
  let tsize = guess_intended_malloc_type stack sizev (weak = Strong) in
  let type_base = type_from_nb_elems tsize in
  let var = create_new_var stack prefix type_base weak in
  Value_parameters.result ~current:true ~once:true
    "allocating %svariable %a%s"
    (if weak = Weak then "weak " else "") Printer.pp_varinfo var
    (if Value_parameters.PrintCallstacks.get ()
     then (Format.asprintf "@.stack: %a" Value_types.Callstack.pretty stack)
     else "");
  let size_char = Bit_utils.sizeofchar () in
  (* Sizes are in bits *)
  let min_alloc = Int.(pred (mul size_char tsize.min_bytes)) in
  let max_alloc = Int.(pred (mul size_char tsize.max_bytes)) in
  (* NOTE: min_alloc/max_alloc may be -1 if the size is zero *)
  assert Int.(ge min_alloc Int.minus_one);
  assert Int.(ge max_alloc min_alloc);
  (* note that min_alloc may be negative (-1) if the allocated size is 0 *)
  let weak = match weak with Weak -> true | Strong -> false in
  let variable_v = Base.create_variable_validity ~weak ~min_alloc ~max_alloc in
  let new_base = Base.register_allocated_var var deallocation (Base.Variable variable_v) in
  register_malloced_base ~stack new_base;
  new_base, max_alloc

(* Simplest allocation function: a new base each time, of the required size. *)
let alloc_fresh ?(prefix="malloc") weak region state actuals =
  match actuals with
  | [_, size, _] ->
    let stack = call_stack_no_wrappers () in
    let base, max_valid = alloc_abstract weak region stack prefix size state in
    let new_state = add_uninitialized state base max_valid in
    let c_values = wrap_fallible_malloc base state new_state in
    { Value_types.c_values = c_values ;
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.NoCacheCallers;
      c_from = None;
    }
  | _ -> raise (Builtins.Invalid_nb_of_args 1)

let () = Builtins.register_builtin "Frama_C_malloc_fresh" (alloc_fresh Strong Base.Malloc)
let () = Builtins.register_builtin "Frama_C_malloc_fresh_weak" (alloc_fresh Weak Base.Malloc)

let alloc_size_ok intended_size =
  try
    let size = Cvalue.V.project_ival intended_size in
    let ok_size =
      Ival.inject_range (Some Integer.zero) (Some (Bit_utils.max_byte_size ()))
    in
    if Ival.is_included size ok_size then Alarmset.True
    else if Ival.intersects size ok_size then Alarmset.Unknown
    else Alarmset.False
  with Cvalue.V.Not_based_on_null -> Alarmset.Unknown (* garbled mix in size *)

(* Generic function used both by [calloc_size] and [calloc_by_stack].
   [calloc_f] is the actual function used (calloc_size or calloc_by_stack). *)
let calloc_abstract calloc_f state actuals =
  let stack = call_stack_no_wrappers () in
  let nmemb, sizev =
    match actuals with
    | [(_exp, nmemb, _); (_, size, _)] -> nmemb, size
    | _ -> raise (Builtins.Invalid_nb_of_args 2)
  in
  let alloc_size = Cvalue.V.mul nmemb sizev in
  let size_ok = alloc_size_ok alloc_size in
  if size_ok <> Alarmset.True then
    Value_util.warning_once_current
      "calloc out of bounds: assert(nmemb * size <= SIZE_MAX)";
  if size_ok = Alarmset.False then (* size always overflows *)
    { Value_types.c_values = [Eval_op.wrap_ptr Cvalue.V.singleton_zero, state];
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.NoCacheCallers;
      c_from = None;
    }
  else
    let base, max_valid = calloc_f stack "calloc" alloc_size state in
    let new_state = add_zeroes state base max_valid in
    let returns_null = if size_ok = Alarmset.Unknown then Some true else None in
    let c_values = wrap_fallible_malloc ?returns_null base state new_state in
    { Value_types.c_values = c_values ;
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.NoCacheCallers;
      c_from = None;
    }

(* Equivalent to [malloc_fresh], but for [calloc]. *)
let calloc_fresh weak state actuals =
  calloc_abstract (alloc_abstract weak Base.Malloc) state actuals

let () = Builtins.register_builtin "Frama_C_calloc_fresh" (calloc_fresh Strong)
let () = Builtins.register_builtin "Frama_C_calloc_fresh_weak" (calloc_fresh Weak)

(* Variables that have been returned by a call to an allocation function
   at this callstack. The first allocated variable is at the top of the
   stack. Currently, the callstacks are truncated according to
   [-val-malloc-functions]. *)
module MallocedByStack = (* varinfo list Callstack.hashtbl *)
  State_builder.Hashtbl(Value_types.Callstack.Hashtbl)
    (Datatype.List(Base))
    (struct
       let name = "Value.Builtins_malloc.MallocedByStack"
       let size = 17
       let dependencies = [Ast.self]
     end)
let () = Ast.add_monotonic_state MallocedByStack.self

(* Performs an abstract allocation on an existing allocated variable,
   its validity. If [make_weak], the variable is marked as being weak. *)
let update_variable_validity ?(make_weak=false) base sizev =
  let size_min, size_max = extract_size sizev in
  match base with
  | Base.Allocated (vi, _deallocation, (Base.Variable variable_v)) ->
    if make_weak && (variable_v.Base.weak = false) then
      mutate_name_to_weak vi;
    let min_sure_bits = Int.(pred (mul eight size_min)) in
    let max_valid_bits = Int.(pred (mul eight size_max)) in
    if not (Int.equal variable_v.Base.min_alloc min_sure_bits) ||
       not (Int.equal variable_v.Base.max_alloc max_valid_bits)
    then begin
      Value_parameters.result ~dkey ~current:true ~once:false
        "@[resizing variable `%a'@ (%a) to fit %a@]"
        Printer.pp_varinfo vi
        pp_validity (variable_v.Base.min_alloc, variable_v.Base.max_alloc)
        pp_validity (min_sure_bits, max_valid_bits);
      (* Mutating the type of a varinfo is not exactly a good idea. This is
         probably fine here, because the type of a malloced variable is
         almost never used. *)
      vi.vtype <- weaken_type vi.vtype;
    end;
    Base.update_variable_validity variable_v
      ~weak:make_weak ~min_alloc:min_sure_bits ~max_alloc:max_valid_bits;
    base, max_valid_bits
  | _ -> Value_parameters.fatal "base is not Allocated: %a" Base.pretty base

let alloc_by_stack_aux region stack prefix sizev state =
  let max_level = Value_parameters.MallocLevel.get () in
  let all_vars =
    try MallocedByStack.find stack
    with Not_found -> []
  in
  let rec aux nb vars =
    match vars with
    | [] -> (* must allocate a new variable *)
      let b, _ as r = alloc_abstract Strong region stack prefix sizev state in
      MallocedByStack.replace stack (all_vars @ [b]);
      r
    | b :: q ->
      try
        ignore (Model.find_base b state);
        if nb = max_level then begin (* variable already used *)
          update_variable_validity ~make_weak:true b sizev
        end
        else aux (nb+1) q
      with Not_found -> (* Can reuse this (strong) variable *)
        update_variable_validity ~make_weak:false b sizev
  in
  aux 0 all_vars

(* For each callstack, the first MallocPrecision.get() are precise fresh
   distinct locations. The following allocations all return the same
   base, first strong, then weak, and which is extended as needed. *)
let alloc_by_stack ?(prefix="malloc") region ?returns_null : Db.Value.builtin_sig = fun state actuals->
  let stack = call_stack_no_wrappers () in
  let sizev = match actuals with
    | [_,size,_] -> size
    | _ -> raise (Builtins.Invalid_nb_of_args 1)
  in
  let base, max_valid = alloc_by_stack_aux region stack prefix sizev state in
  let new_state = add_uninitialized state base max_valid in
  let c_values = wrap_fallible_malloc ?returns_null base state new_state in
  { Value_types.c_values = c_values ;
    c_clobbered = Base.SetLattice.bottom;
    c_from = None;
    c_cacheable = Value_types.NoCacheCallers }
;;
let () = Builtins.register_builtin
    ~replace:"malloc" "Frama_C_malloc_by_stack" (alloc_by_stack Base.Malloc)
let () = Builtins.register_builtin
    ~replace:"__fc_vla_alloc" "Frama_C_vla_alloc_by_stack"
    (alloc_by_stack Base.VLA ~returns_null:false)
let () = Builtins.register_builtin
    ~replace:"alloca" "Frama_C_alloca"
    (alloc_by_stack ~prefix:"alloca" Base.Alloca ~returns_null:false)

(* Equivalent to [alloc_by_stack], but for [calloc]. *)
let calloc_by_stack : Db.Value.builtin_sig = fun state actuals ->
  calloc_abstract (alloc_by_stack_aux Base.Malloc) state actuals

let () = Builtins.register_builtin
    ~replace:"calloc" "Frama_C_calloc_by_stack" calloc_by_stack

(** {1 Free} *)

(* Change all references to bases into ESCAPINGADDR into the given state,
   and remove those bases from the state entirely when [exact] holds *)
let free ~exact bases state =
  let changed = ref Locations.Zone.bottom in
  (* Uncomment this code to simulate the fact that free "writes" the bases
     it deallocates
  Base_hptmap.iter (fun b ->
      changed := Zone.join !changed (enumerate_bits (loc_of_base b))
    ) bases; *)
  (* No need to remove the freed bases from the state if [exact] is false,
     because they must remain for the 'inexact' case *)
  let state =
    if exact then Base.Hptset.fold Cvalue.Model.remove_base bases state
    else state
  in
  let escaping = bases in
  let on_escaping ~b ~itv ~v:_ =
    let z = Locations.Zone.inject b (Int_Intervals.inject_itv itv) in
    changed := Locations.Zone.join !changed z
  in
  let within = Base.SetLattice.top in
  let state =
    Locals_scoping.make_escaping ~exact ~escaping ~on_escaping ~within state
  in
  let from_changed =
    let open Function_Froms in
    let m = Memory.(add_binding ~exact empty !changed Deps.bottom) in
    { deps_table = m; deps_return = Deps.bottom }
  in
  state, (from_changed, if exact then !changed else Zone.bottom)

let resolve_bases_to_free arg =
  (* Categorizes the bases in arg *)
  let f base offset (acc, card, null) =
    let allocated_base = Base_hptmap.mem base (Dynamic_Alloc_Bases.get ()) in
    (* Does arg contain at least one invalid value? *)
    if (not allocated_base && not (Base.is_null base))
      || Ival.contains_non_zero offset
      || is_automatically_deallocated base
    then Value_util.warning_once_current
      "Wrong free: assert(pass a freeable address)";
    (* Collect the bases to remove from the memory state.
       Also count the number of freeable bases (including NULL). *)
    if Ival.contains_zero offset
    then begin
      let base_card = match Base.validity base with
        | Base.Variable { Base.weak = true } -> 2
          (* weak validity has "infinite" cardinality; but here we use 2 since
             any value > 1 leads to a weak update anyway *)
        | _ -> 1
      in
      if allocated_base
      then Base.Hptset.add base acc, card + base_card, null
      else if Base.is_null base
      then acc, card + base_card, true
      else acc, card, null
      end
    else acc, card, null
  in
  Cvalue.V.fold_topset_ok f arg (Base.Hptset.empty, 0, false)

let free_aux state ~strong bases_to_remove  =
  (* TODO: reduce on arg if it is an lval *)
  if strong then begin
    Value_parameters.debug ~current:true ~dkey "strong free on bases: %a"
      Base.Hptset.pretty bases_to_remove;
    free ~exact:true bases_to_remove state
  end else begin
    Value_parameters.debug ~current:true ~dkey "weak free on bases: %a"
      Base.Hptset.pretty bases_to_remove;
    free ~exact:false bases_to_remove state
  end

(* Builtin for [free] function *)
let frama_c_free state actuals =
  match actuals with
  | [ _, arg, _ ] ->
    let bases_to_remove, card_to_remove, _null = resolve_bases_to_free arg in
    if card_to_remove = 0 then 
      { Value_types.c_values = [];
        c_clobbered = Base.SetLattice.bottom;
        c_from = None;
        c_cacheable = Value_types.Cacheable; }
    else
      let strong = card_to_remove <= 1 in
      let state, changed = free_aux state ~strong bases_to_remove in
      { Value_types.c_values = [None, state];
        c_clobbered = Base.SetLattice.bottom;
        c_from = Some changed;
        c_cacheable = Value_types.Cacheable;
      }
  | _ -> raise (Builtins.Invalid_nb_of_args 1)

let () = Builtins.register_builtin ~replace:"free" "Frama_C_free" frama_c_free

(* built-in for [__fc_vla_free] function. By construction, VLA should always
   be mapped to a single base. *)
let frama_c_vla_free state actuals =
  match actuals with
  | [ _, arg, _] ->
    let bases_to_remove, _card_to_remove, _null = resolve_bases_to_free arg in
    let state, changed = free_aux state ~strong:true bases_to_remove in
    { Value_types.c_values = [None, state];
      c_clobbered = Base.SetLattice.bottom;
      c_from = Some changed;
      c_cacheable = Value_types.Cacheable;
    }
  | _ -> raise (Builtins.Invalid_nb_of_args 1)

let () =
  Builtins.register_builtin
    ~replace:"__fc_vla_free" "Frama_C_vla_free" frama_c_vla_free

let free_automatic_bases stack state =
  (* free automatic bases that were allocated in the current function *)
  let bases_to_free =
    Base_hptmap.fold (fun base stack' acc ->
        if is_automatically_deallocated base &&
           Value_types.Callstack.equal stack stack'
        then Base.Hptset.add base acc
        else acc
      ) (Dynamic_Alloc_Bases.get ()) Base.Hptset.empty
  in
  if Base.Hptset.is_empty bases_to_free then state
  else begin
    Value_parameters.result ~current:true ~once:true
      "freeing automatic bases: %a" Base.Hptset.pretty bases_to_free;
    let state', _changed = free_aux state ~strong:true bases_to_free in
    (* TODO: propagate 'freed' bases for From? *)
    state'
  end

(** {1 Realloc} *)

(* Note: realloc never fails during read/write operations, hence we can
   always ignore the validity of locations. (We craft them ourselves anyway.)
   The only possible cause of failure is a pointer that was not malloced. *)

(* Auxiliary function for [realloc], that copies the [size] first bytes of
   [b] (or less if [b] is too small) in [src_state], then pastes them in
   [new_base] in [dst_state], which is supposed to be big enough for [size].
   This function always perform weak updates, in case multiple bases are
   copied to [new_base]. *)
let realloc_copy_one size ~src_state ~dst_state new_base b =
  let size_char = Bit_utils.sizeofchar () in
  let size_bits = Integer.mul size size_char in
  let up = match Base.validity b with
    | Base.Known (_, up) | Base.Unknown (_, _, up)
    | Base.Variable { Base.max_alloc = up } -> up
    | Base.Invalid | Base.Empty -> Integer.zero

  in
  let size_to_copy = Int.min (Int.succ up) size_bits in
  let src = Location_Bits.inject b Ival.zero in
  match Cvalue.Model.copy_offsetmap src size_to_copy src_state with
  | `Bottom -> assert false
  | `Value offsetmap ->
    if Int.gt size_to_copy Int.zero then
      Cvalue.Model.paste_offsetmap
        ~from:offsetmap ~dst_loc:new_base ~size:size_to_copy
        ~exact:false dst_state
    else dst_state

(* Auxiliary function for [realloc], that performs the allocation of a new
   variable, and copy the pointers being reallocated inside the new base.
   [size] is the size to realloc. [bases_to_realloc] are the pointers to the
   memory to copy. [null_in_arg] indicates that [realloc] was called with
   [null] in its argument. [weak] indicates which type of variable must
   be created: if [Weak], convergence is ensured using a malloc builtin
   that converges.  If [Strong], a new base is created for each call. *)
let realloc_alloc_copy weak bases_to_realloc null_in_arg sizev state =
  Value_parameters.debug ~dkey "bases_to_realloc: %a"
    Base.Hptset.pretty bases_to_realloc;
  assert (not (Model.(equal state bottom || equal state top)));
  let _size_valid, size_max = extract_size sizev in (* bytes everywhere *)
  let stack = call_stack_no_wrappers () in
  let base, max_valid =
    let prefix = "realloc" in
    match weak with
    | Strong -> alloc_abstract Strong Base.Malloc stack prefix sizev state
    | Weak -> alloc_by_stack_aux Base.Malloc stack prefix sizev state
  in
  (* Make sure that [ret] will be present in the result: we bind it at least
     to bottom everywhere *)
  let dst_state = add_uninitialized state base Int.minus_one in
  let ret = V.inject base Ival.zero in
  let loc_bits = Locations.loc_bytes_to_loc_bits ret in
  (* get bases to free and copy *)
  let lbases = Base.Hptset.elements bases_to_realloc in
  let dst_state =
    (* uninitialized on all reallocated valid bits *)
    let offsm = offsm_with_v V_Or_Uninitialized.uninitialized (Base.validity base) max_valid in
    let offsm =
      if null_in_arg then offsm (* In this case, realloc may copy nothing *)
      else
        (* Compute the maximal size that is guaranteed to be copied across all
           bases *)
        let aux_valid size b = Integer.min size (size_sure_valid b) in
        let size_new_loc = Integer.mul size_max (Bit_utils.sizeofchar ()) in
        let size_sure_valid = List.fold_left aux_valid size_new_loc lbases in
        (* Replace the bits [0..size_sure_valid] by [bottom]. Those [bottom]
           will be overwritten in the call to [realloc_copy_one]. *)
        if Int.gt size_sure_valid Int.zero then
          V_Offsetmap.add (Int.zero, Int.pred size_sure_valid)
            (V_Or_Uninitialized.bottom, Int.one, Rel.zero) offsm
        else offsm
    in
    Cvalue.Model.paste_offsetmap
      ~from:offsm ~dst_loc:loc_bits ~size:(Int.succ max_valid) ~exact:false
     dst_state
  in
  (* Copy the old bases *)
  let copy_one dst_state b =
    realloc_copy_one size_max ~src_state:state ~dst_state loc_bits b
  in
  let state = List.fold_left copy_one dst_state lbases in
  ret, state

(* Auxiliary function for [realloc]. All the bases in [bases] are realloced
   one by one, plus NULL if [null] holds. This function acts as if we had
   first made a disjunction on the pointer passed to [realloc]. *)
let realloc_multiple state size bases null =
  (* this function should never be used with weak allocs *)
  let aux_bases b acc = Base.Hptset.singleton b :: acc in
  let lbases = Base.Hptset.fold aux_bases bases [] in
  (* This function reallocates the base [b] alone, but does not free it.
     We cannot free yet, because [b] would leak in the states corresponding
     to the variables different from [b]. *)
  let realloc_one_base b = realloc_alloc_copy Strong b false size state in
  let join (ret1, st1) (ret2, st2) = V.join ret1 ret2, Model.join st1 st2 in
  let aux_one_base acc b = join (realloc_one_base b) acc in
  let res = List.fold_left aux_one_base (V.bottom, state) lbases in
  (* Add another base for realloc(NULL) if needed. *)
  if null then
    join res (realloc_alloc_copy Strong Base.Hptset.empty true size state)
  else res

(* Multiple indicates that existing bases are reallocated into as many new
   bases. *)
let realloc ~multiple state args = match args with
  | [ (_,ptr,_); (_,size,_) ] ->
    let (bases, card_ok, null) = resolve_bases_to_free ptr in
    if card_ok > 0 then
      let ret, state =
        if multiple
        then realloc_multiple state size bases null
        else realloc_alloc_copy Weak bases null size state
      in
      (* Maybe the calls above made [ret] weak, and it
         was among the arguments. In this case, do not free it entirely! *)
      let weak = Base.Hptset.exists Base.is_weak bases in
      let strong = card_ok <= 1 && not weak in
      (* free old bases. *)
      let state, changed = free_aux state ~strong bases in
      { Value_types.c_values = [Eval_op.wrap_ptr ret, state] ;
        c_clobbered = Builtins.clobbered_set_from_ret state ret;
        c_cacheable = Value_types.NoCacheCallers;
        c_from = Some changed;
      }
    else (* Invalid call. *)
      { Value_types.c_values = [] ;
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCacheCallers;
        c_from = None;
      }
  | _ -> raise (Builtins.Invalid_nb_of_args 2)

let () = Builtins.register_builtin
    ~replace:"realloc" "Frama_C_realloc" (realloc ~multiple:false)
let () = Builtins.register_builtin
    "Frama_C_realloc_multiple" (realloc ~multiple:true)


(** {1 Leak detection} *)

(* Experimental, not to be released, leak detection built-in. *)
(* Check if the base_to_check is present in one of 
   the offsetmaps of the state *)
exception Not_leaked
let check_if_base_is_leaked base_to_check state =
  match state with
  | Model.Bottom -> false
  | Model.Top -> true
  | Model.Map m -> 
  try 
    Cvalue.Model.fold 
      (fun base offsetmap () -> 
	if not (Base.equal base_to_check base) then 
	  Cvalue.V_Offsetmap.iter_on_values 
	    (fun v ->
	      if Locations.Location_Bytes.may_reach base_to_check 
		(V_Or_Uninitialized.get_v v) then raise Not_leaked) 
	    offsetmap)
      m
      ();
    true
  with Not_leaked -> false

(* Does not detect leaked cycles within malloc'ed bases.
   The complexity is very far from being optimal. *)
let check_leaked_malloced_bases state _ = 
  let alloced_bases = Dynamic_Alloc_Bases.get () in
  Base_hptmap.iter
    (fun base _ -> if check_if_base_is_leaked base state then
	Value_util.warning_once_current "memory leak detected for %a"
	  Base.pretty base)
    alloced_bases;
  { Value_types.c_values = [None,state] ;
    c_clobbered = Base.SetLattice.bottom;
    c_cacheable = Value_types.NoCacheCallers;
    c_from = None;
  }

let () = 
  Builtins.register_builtin "Frama_C_check_leak" check_leaked_malloced_bases


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
