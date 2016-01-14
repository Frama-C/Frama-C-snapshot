(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Creation of the initial state for Value *)

open Cil_types
open Abstract_interp
open Cvalue
open Locations
open Value_util

let dkey = Value_parameters.register_category "initial_state"

exception Initialization_failed

(** Those functions intentionally ignore 'const' attributes. Functions of
    Eval_op should not be used in this module, unless they have a 'reducing'
    argument. *)
let add_initialized state loc v =
  Cvalue.Model.add_initial_binding state loc (V_Or_Uninitialized.initialized v)

let make_well hidden_base state loc =
  let size = Bit_utils.max_bit_size () in
  let well =
    Cvalue.V.inject_top_origin Origin.Well (Base.Hptset.singleton hidden_base)
  in
  let well_loc =
    Locations.make_loc
      (Location_Bits.inject hidden_base Ival.zero)
      (Int_Base.inject size)
  in
  let state = add_initialized state well_loc well in
  add_initialized state loc well


let warn_unknown_size_aux pp v (messt, t) =
  Value_parameters.warning ~once:true ~current:true
    "@[during initialization@ of %a,@ size of@ type '%a'@ cannot be@ computed@ \
    (%s)@]" pp v Printer.pp_typ t messt

let warn_unknown_size =
  warn_unknown_size_aux
    (fun fmt v -> Format.fprintf fmt "variable '%a'" Printer.pp_varinfo v)

type validity_hidden_base =
  | UnknownValidity (* Base is maybe invalid on its entire validity *)
  | KnownThenUnknownValidity of Integer.t (* Base is valid on i bits, then
                              maybe invalid on the remainder of its validity *)

let create_hidden_base ~valid ~hidden_var_name ~name_desc pointed_typ =
  let hidden_var = Value_util.create_new_var hidden_var_name pointed_typ in
  hidden_var.vdescr <- Some name_desc;
  let validity =
    (* Add a special case for void* pointers: we do not want to compute the
       size of void *)
    let validity = match Cil.unrollType pointed_typ with
      | TVoid _ -> Base.Unknown (Int.zero, None, Bit_utils.max_bit_address ())
      | _ -> Base.validity_from_type hidden_var
    in
    match validity with
    | Base.Known (a,b)
      when not (Value_parameters.AllocatedContextValid.get ()) ->
      (* Weaken validity, because the created variables are not supposed
         to be valid *)
      (match valid with
       | KnownThenUnknownValidity size -> (*except here, for size bits*)
         let size = Integer.pred size in
         assert (Integer.le size b);
         Base.Unknown (a, Some size, b)
       | UnknownValidity -> Base.Unknown (a, None, b)
      )
    | Base.Unknown _ -> (* Unknown validity is caused by strange type *)
      Value_parameters.result ~dkey "creating variable %s with imprecise \
          size (type %a)" hidden_var_name Printer.pp_typ pointed_typ;
      validity
    | Base.Known _ | Base.Invalid -> validity
  in
  Base.register_memory_var hidden_var validity

(* Alternative version of the code in {!Locations}, but we catch 0 size
   explicitly and raise an error. *)
let loc_of_typoffset b typ offset =
  try
    let offs, size = Cil.bitsOffset typ offset in
    if size = 0 then
      Value_parameters.abort ~current:true
        "@[Zero-sized@ location %a%a@ (type '%a').@ Aborting@]"
        Base.pretty b Printer.pp_offset
        offset Printer.pp_typ (Cil.typeOffset typ offset);
    let size = Int_Base.inject (Int.of_int size) in
    Locations.make_loc (Location_Bits.inject b (Ival.of_int offs)) size
  with Cil.SizeOfError _ as _e ->
    Locations.make_loc (Location_Bits.inject b Ival.top) Int_Base.top

let reject_empty_struct b offset typ =
  match Cil.unrollType typ with
  | TComp (ci, _, _) ->
    if ci.cfields = [] && ci.cdefined then
      Value_parameters.abort ~current:true
        "@[empty %s@ are unsupported@ (type '%a',@ location %a%a).@ Aborting@]"
        (if ci.cstruct then "struct" else "union")
        Printer.pp_typ typ Base.pretty b Printer.pp_offset offset
  | _ -> ()


(** [initialize_var_using_type varinfo state] uses the type of [varinfo]
    to create an initial value in [state]. *)
let initialize_var_using_type varinfo state =
  let with_alarms = CilE.warn_none_mode in
  Cil.CurrentLoc.set varinfo.vdecl;
  let rec add_offsetmap depth b name_desc name typ offset_orig typ_orig state =
    let typ = Cil.unrollType typ in
    let loc = lazy (loc_of_typoffset b typ_orig offset_orig) in
    let bind_entire_loc ?(state=state) v = (* Shortcut *)
      add_initialized state (Lazy.force loc) v
    in
      match typ with
      | TInt _ | TEnum (_, _)->
          bind_entire_loc Cvalue.V.top_int
            
      | TFloat (fkind, _) -> begin
        match Value_util.float_kind fkind with
        | Fval.Float32 ->
          bind_entire_loc Cvalue.V.top_single_precision_float
        | Fval.Float64 ->
          bind_entire_loc Cvalue.V.top_float
      end

      | TFun _ -> state

      | TPtr (typ, _) as full_typ
          when depth <= Value_parameters.AutomaticContextMaxDepth.get () ->
          let attr = Cil.typeAttr full_typ in
          let context_max_width =
            Value_parameters.AutomaticContextMaxWidth.get ()
          in begin
          match Cil.isVoidType typ, Cil.isFunctionType typ with
          | false, false -> (* non-void, non-function *)
            let i =
              match Cil.findAttribute "arraylen" attr with
              | [AInt i] -> i
              | _ -> Integer.of_int context_max_width
            in
            let arr_pointed_typ =
              TArray(typ,
                     Some (Cil.kinteger64 ~loc:varinfo.vdecl i),
                     Cil.empty_size_cache (),
                     [])
            in
            let hidden_var_name =
              Cabs2cil.fresh_global ("S_" ^ name)
            in
            let name_desc = "*"^name_desc in
            (* Make first cell of the array valid. The NULL pointer takes
               care of a potential invalid pointer. *)
            let valid =
              try KnownThenUnknownValidity (Integer.of_int (Cil.bitsSizeOf typ))
              with Cil.SizeOfError _ -> UnknownValidity
            in
            let hidden_base =
              create_hidden_base
                ~valid ~hidden_var_name ~name_desc arr_pointed_typ
            in
            let state =
              add_offsetmap
                (depth + 1)
                hidden_base
                name_desc
                hidden_var_name
                arr_pointed_typ
                NoOffset
                arr_pointed_typ
                state
            in
            let value = Cvalue.V.inject hidden_base (Ival.zero) in
            let value =
              if Value_parameters.AllocatedContextValid.get ()
              then value
              else Cvalue.V.join Cvalue.V.singleton_zero value
            in
            bind_entire_loc ~state value
          | true, false -> (* void *)
            let hidden_var_name = Cabs2cil.fresh_global ("S_" ^ name) in
            let name_desc = "*"^name_desc in
            let valid = UnknownValidity in
            let hidden_base =
              create_hidden_base ~valid ~hidden_var_name ~name_desc typ
            in
            make_well hidden_base state (Lazy.force loc)
          | false, true -> (* function *)
            (* Generating functions is next to useless for the user (what
               does the function do), and too dangerous for the AST. *)
            bind_entire_loc ~state V.singleton_zero
          | true, true -> assert false (* inconsistent *)
          end

      | TArray (typ, len, _, _) ->
          begin try
            let size = Cil.lenOfArray len in
            let size_elt = Int.of_int (Cil.bitsSizeOf typ) in
            let psize = pred size in
            let state = ref state in
            let typ = Cil.unrollType typ in
            let max_precise_size =
              Value_parameters.AutomaticContextMaxWidth.get ()
            in
            let locs = ref [] in
            for i = 0 to min psize (pred max_precise_size) do
              (* Cells that are treated really precisely. We create new
                 pointers (if needed) for each distinct cell *)
              let offset =
                Cil.addOffset
                  (Index (Cil.integer ~loc:varinfo.vdecl i, NoOffset))
                  offset_orig
              in
              let name = string_of_int i ^ "_" ^ name in
              let name_desc = name_desc ^ "[" ^ string_of_int i ^ "]" in
              state :=
                add_offsetmap depth b name_desc name typ offset typ_orig !state;
	      let loc = loc_of_typoffset b typ_orig offset in
              locs := loc :: !locs;
            done;
            if max_precise_size < size then begin
              (* Some elements remain to be initialized *)
              let offsm_of_loc loc = (* This rereads one of the first cells*)
                let _alarm, offsm =
                  Cvalue.Model.copy_offsetmap loc size_elt !state
                in
                match offsm with `Bottom | `Top -> assert false | `Map m -> m
              in
              let last_loc, locs = match !locs with
                | [] -> assert false (* AutomaticContextMaxWidth is at least 1*)
                | l :: ll -> l, ll
              in
	      let last_offsm = offsm_of_loc last_loc.loc in
              (* Join of the contents of the first elements *)
              let aux_loc offsm loc =
                Cvalue.V_Offsetmap.join offsm (offsm_of_loc loc.loc)
              in
              let offsm_joined = List.fold_left aux_loc last_offsm locs in
              (* TODO: add Offsetmap.paste_repeated_slices to Offsetmap, and
                 replace everything below by a call to it. *)
              let nb_fields =
		Cvalue.V_Offsetmap.fold (fun _itv _ -> succ) offsm_joined 0
              in
              if nb_fields = 1 then
                (* offsm_joined is very regular (typically Top_int, or some
                  pointers). We read its contents and copy it everywhere else.
                  The periodicity of the contents may be smaller than the size
                   of a cell; take this into account. *)
                let v, modu, offset =
                  Extlib.the (Cvalue.V_Offsetmap.fold
                                (fun _itv v _ -> Some v) offsm_joined None)
                in
                assert (Rel.equal offset Rel.zero);
                let ncells = size - max_precise_size in
                let total_size = Int.mul size_elt (Int.of_int ncells) in
                let offsm_repeat = V_Offsetmap.create
                  ~size_v:modu ~size:total_size v in
                let loc = Location_Bits.shift
                  (Ival.inject_singleton size_elt) last_loc.loc;
                in
                (* paste [size - max_precise_size] elements, starting from
                   the last location initialized + 1 *)
                state :=
                  Eval_op.paste_offsetmap ~reducing:true ~with_alarms
                    ~from:offsm_repeat
                    ~dst_loc:loc
                    ~size:total_size
                    ~exact:true
                    !state
              else (
                (* We have probably initialized a struct with different fields.
                   We must perform offsetmap copies, that are slower *)
                if nb_fields * psize >= 5000 then
                  Value_parameters.result ~once:true ~current:true
                    "Initializing a complex array of %d elements. This may \
                       take some time" size;
                let loc = ref last_loc.loc in
                for _i = max_precise_size to psize do
                  loc := Location_Bits.shift
                    (Ival.inject_singleton size_elt) !loc;
                  state :=
                    Eval_op.paste_offsetmap ~reducing:true ~with_alarms
                      ~from:offsm_joined
                      ~dst_loc:!loc
                      ~size:size_elt
                      ~exact:true
                      !state
                done);
            end;
            !state
          with
            | Cil.LenOfArray ->
                Value_parameters.result ~once:true ~current:true
                  "no size specified for array, assuming 0";
                (* This is either a flexible array member (for which Cil
                 implicitely returns a size of 0, so we are doing the proper
                 thing), or an incomplete array (which is forbidden)  *)
                state
            | Cil.SizeOfError (s, t) ->
                warn_unknown_size varinfo (s, t);
                bind_entire_loc Cvalue.V.top_int;
          end

      | TComp ({cstruct=true;} as compinfo, _, _) -> (* Struct *)
          reject_empty_struct b offset_orig typ;
          let treat_field state field =
            match field.fbitfield with
            | Some 0 -> state (* skip the field, nothing to initialize *)
            | _ ->
              let new_offset = Field (field, NoOffset) in
              let offset = Cil.addOffset new_offset offset_orig in
              let nd = name_desc ^ "." ^ field.fname in
              let n = field.fname ^ "_" ^ name in
              add_offsetmap depth b nd n field.ftype offset typ_orig state
          in
          begin
            try
              List.fold_left treat_field state compinfo.cfields
            with Cil.SizeOfError (s, t) ->
              warn_unknown_size varinfo (s, t);
              bind_entire_loc Cvalue.V.top_int;
          end

      | TComp ({cstruct=false}, _, _) when Cil.is_fully_arithmetic typ ->
          reject_empty_struct b offset_orig typ;
          (* Union of arithmetic types *)
          bind_entire_loc Cvalue.V.top_int

      | TPtr _ when Value_parameters.AllocatedContextValid.get () ->
          (* deep pointers map to NULL in this case *)
          bind_entire_loc Cvalue.V.singleton_zero

      | TBuiltin_va_list _ | TComp _ | TVoid _  | TPtr  _ ->
          reject_empty_struct b offset_orig typ;
          (* variable arguments or union with non-arithmetic type
             or deep pointers *)
          (* first create a new varid and offsetmap for the
             "hidden location" *)
          let hidden_var_name =
            Cabs2cil.fresh_global ("WELL_"^name)
          in
          let hidden_var =
            Value_util.create_new_var hidden_var_name Cil.charType
          in
          hidden_var.vdescr <- Some (name_desc^"_WELL");
          let validity = Base.Known (Int.zero, Bit_utils.max_bit_address ()) in
          let hidden_base = Base.register_memory_var hidden_var validity in
          make_well hidden_base state (Lazy.force loc)
      | TNamed (_, _)  -> assert false
  in
  add_offsetmap
    0
    (Base.of_varinfo varinfo)
    varinfo.vname varinfo.vname varinfo.vtype NoOffset varinfo.vtype state


(** Fill [vi] everywhere with padding. The exact contents (bottom | zero |
    top_int), initialized or not, is determined from [lib_entry] and option
    [-val-initialization-padding-globals] *)
let init_var_padding ~lib_entry vi state = 
  let loc = Locations.loc_of_varinfo vi in
  match Value_parameters.InitializationPaddingGlobals.get () with
  | "yes" ->
    let v = if lib_entry then V.top_int else V.singleton_zero in
    Cvalue.Model.add_initial_binding state loc
      (V_Or_Uninitialized.C_init_noesc v)
  | "no" ->
    Cvalue.Model.add_initial_binding state loc
      V_Or_Uninitialized.uninitialized
  | "maybe" ->
    let v = if lib_entry then V.top_int else V.singleton_zero in
    Cvalue.Model.add_initial_binding state loc
      (V_Or_Uninitialized.C_uninit_noesc v)
  | _ -> assert false

let warn_size vi =
  try
    ignore (Cil.bitsSizeOf vi.vtype);
    false
  with Cil.SizeOfError (s, t)->
    warn_unknown_size vi (s, t);
    true

let init_var_zero vi state =
  ignore (warn_size vi);
  let loc = Locations.loc_of_varinfo vi in
  add_initialized state loc V.singleton_zero

let init_var_volatile vi state =
  ignore (warn_size vi);
  let loc = Locations.loc_of_varinfo vi in
  add_initialized state loc V.top_int

let init_var_lib_entry vi state =
  let loc = Locations.loc_of_varinfo vi in
  if warn_size vi then
    add_initialized state loc V.top_int
  else
    (* add padding everywhere *)
    let state = init_var_padding ~lib_entry:true vi state in
    (* then initialize non-padding bits according to the type *)
    initialize_var_using_type vi state


(* Is the padding filled with fully initialized values. In this case, we
   can speed up the generation of the initial state in a few cases. *)
let fully_initialized_padding () =
  Value_parameters.InitializationPaddingGlobals.get () = "yes"

let eval_lval_to_loc lval =
  let with_alarms = CilE.warn_none_mode in
  (* Eval in Top state. We do not want the location to depend on other globals*)
  let _, loc, typ_lval =
    Eval_exprs.lval_to_loc_state ~with_alarms Cvalue.Model.top lval
  in
  loc, typ_lval

(* Evaluation of a [SingleInit] in Cil parlance *)
let init_single_initializer state lval exp =
  let loc, typ_lval = eval_lval_to_loc lval in
  let value =
    Eval_exprs.eval_expr ~with_alarms:(warn_all_quiet_mode ()) state exp
  in
  if Cvalue.V.equal value V.bottom then (
    Value_parameters.result ~source:(fst exp.eloc)
      "Evaluation of initializer '%a' failed@." Printer.pp_exp exp;
    raise Initialization_failed);
  let v =
    if Cil.typeHasQualifier "volatile" typ_lval
    then V.top_int
    else Eval_typ.cast_lval_if_bitfield typ_lval loc.Locations.size value
  in
  add_initialized state loc v

(* Apply an initializer (not recursively). Take volatile qualifiers into
   account. If [warn] holds, we warn when an initializer is ignored
   because it points to a volatile location. *)
let rec init_initializer_or_volatile state lval init warn =
  if Cil.typeHasQualifier "volatile" (Cil.typeOfLval lval) then begin
    if warn then
      warning_once_current "global initialization of volatile zone %a ignored"
        Printer.pp_lval lval;
    let loc, _ = eval_lval_to_loc lval in
    add_initialized state loc V.top_int
  end
  else
    match init with
    | SingleInit exp -> init_single_initializer state lval exp
    | CompoundInit (base_typ, l) ->
      Cil.foldLeftCompound
        ~implicit:false
        ~doinit:
        (fun off init _typ state ->
          let lval' = Cil.addOffsetLval off lval in
          init_initializer_or_volatile state lval' init warn)
        ~ct:base_typ
        ~initl:l
        ~acc:state

(* Special initializers. Only lval with attributes 'const' and non-volatile
   are initialized *)
let rec init_const_initializer state lval init =
  match init with
    | SingleInit exp ->
      let typ_lval = Cil.typeOfLval lval in
      if Cil.typeHasQualifier "const" typ_lval &&
        not (Cil.typeHasQualifier "volatile" typ_lval)
      then
        init_single_initializer state lval exp
      else state

    | CompoundInit (base_typ, l) ->
      if Cil.typeHasQualifier "volatile" base_typ ||
        not (Cil.typeHasAttributeDeep "const" base_typ)
      then state (* initializer is not useful *)
      else
        Cil.foldLeftCompound
          ~implicit:true
          ~doinit:
          (fun off init _typ state ->
            init_const_initializer state (Cil.addOffsetLval off lval) init)
          ~ct:base_typ
          ~initl:l
          ~acc:state

(** Bind the declared range for NULL to top int *)
let initialize_null state =
  let min_valid = Base.min_valid_absolute_address () in
  let max_valid = Base.max_valid_absolute_address () in
  if Int.le min_valid max_valid
  then begin
    (* Bind everything between [0..max] to bottom. Offsetmaps cannot
       contain holes, which can happen when min > 0 holds. *)
    let bot = V_Offsetmap.create_isotropic
      ~size:max_valid (V_Or_Uninitialized.initialized V.bottom)
    in
    let v = if true (* TODO: command line option *)
      then V_Or_Uninitialized.initialized V.top_int
      else V_Or_Uninitialized.uninitialized
    in
    let offsm =
      V_Offsetmap.add (min_valid, max_valid) (v, Int.one, Rel.zero) bot
    in
    Cvalue.Model.add_base Base.null offsm state
  end
  else state

(* initialize [vi] when [-lib-entry] is not set, by writing successively
   the padding, zero, and the initializers. *)
let init_var_not_lib_entry_initializer vi init state =
  Cil.CurrentLoc.set vi.vdecl;
  let volatile_somewhere = Cil.typeHasAttributeDeep "volatile" vi.vtype in
  let volatile_everywhere = Cil.typeHasQualifier "volatile" vi.vtype in
  if fully_initialized_padding () &&
    (volatile_everywhere || not volatile_somewhere)
  then
    (* shortcut: padding and volatile won't interfere, we can do a global
       initialisation, then write the initializer on top if there is one. *)
    if volatile_everywhere then begin
      if init <> None then
        warning_once_current
          "global initialization of volatile variable %a ignored"
          Printer.pp_varinfo vi;
      init_var_volatile vi state
    end
    else
      let state = init_var_zero vi state in
      match init with
      | None -> state
      | Some init ->
        init_initializer_or_volatile state (Var vi,NoOffset) init true
  else (* "slow" initialization *)
    let state = init_var_padding ~lib_entry:false vi state in
    let typ = vi.vtype in
    let loc = Cil_datatype.Location.unknown in
    let zi = Cil.makeZeroInit ~loc typ in
    (* initialise everything (except padding) to zero). Do not warn, as
       most of the initializer is generated. *)
    let state = init_initializer_or_volatile state (Var vi,NoOffset) zi false in
    (* then write the real initializer on top *)
    match init with
    | None -> state
    | Some init ->
      init_initializer_or_volatile state (Var vi,NoOffset) init true


(* initialize [vi] as if in [-lib-entry] mode. Active when [-lib-entry] is set,
   or when [vi] is extern. [const] initializers, explicit or implicit, are
   taken into account *)
let init_var_lib_entry_initializer vi init state =
  Cil.CurrentLoc.set vi.vdecl;
  if Cil.typeHasAttribute "const" vi.vtype && not (vi.vstorage = Extern)
  then (* Fully const base. Ignore -lib-entry altogether *)
    init_var_not_lib_entry_initializer vi init state
  else
    (* Fill padding + contents of non-padding bits according to the type *)
    let state = init_var_lib_entry vi state in
    (* if needed, initialize const fields according to the initialiser
       (or generate one if there are none). In the first phase, they have been
       set to generic values *)
    if Cil.typeHasAttributeDeep "const" vi.vtype && not (vi.vstorage = Extern)
    then
      let init = match init with
        | None -> Cil.makeZeroInit ~loc:vi.vdecl vi.vtype
        | Some init -> init
      in
      init_const_initializer state (Var vi, NoOffset) init
    else state


module NotLibEntryGlobals =
  State_builder.Option_ref
    (Cvalue.Model)
    (struct
      let name = "Value.Initial_state.NotLibEntryGlobals"
      let dependencies =
        [ Ast.self; Kernel.AbsoluteValidRange.self;
          Value_parameters.InitializationPaddingGlobals.self ]
     end)

module LibEntryGlobals =
  State_builder.Option_ref
    (Cvalue.Model)
    (struct
      let name = "Value.Initial_state.LibEntryGlobals"
      open Value_parameters
      let dependencies =
        [ Ast.self; Kernel.AbsoluteValidRange.self;
          InitializationPaddingGlobals.self; AllocatedContextValid.self;
          AutomaticContextMaxWidth.self; AutomaticContextMaxDepth.self;
        ]
     end)
let () = Ast.add_monotonic_state LibEntryGlobals.self

let initial_state ~lib_entry () =
  Value_parameters.debug ~level:2 "Computing globals values";
  try
    Globals.Vars.fold_in_file_order
      (fun vi init state ->
        if vi.vsource then begin
          let initialize = 
            if lib_entry || (vi.vstorage = Extern (* use -lib-entry mode. *)) 
            then init_var_lib_entry_initializer
            else init_var_not_lib_entry_initializer
          in
          initialize vi init.init state
        end
        else state
      ) (initialize_null Cvalue.Model.empty_map)
  with Initialization_failed -> Cvalue.Model.bottom

let initial_state_not_lib_entry () =
  NotLibEntryGlobals.memo (initial_state ~lib_entry:false)

let initial_state_lib_entry () =
  LibEntryGlobals.memo (initial_state ~lib_entry:true)

let () =
  Db.Value.initial_state_only_globals :=
    (fun () ->
      if snd (Globals.entry_point ()) then
        initial_state_lib_entry ()
      else
        initial_state_not_lib_entry ()
    );

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
