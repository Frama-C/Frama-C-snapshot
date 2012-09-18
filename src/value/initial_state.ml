(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

exception Initialization_failed

let typeHasAttribute attr typ = Cil.hasAttribute attr (Cil.typeAttrs typ)

(* If [full] is true, bind the contents of hidden_base to a well of itself.
   If not, do not bind it (typically for function pointers) *)
let make_well ~full hidden_base state loc =
  let size = Bit_utils.max_bit_size () in
  let well = Cvalue.V.inject_top_origin
    Origin.Well
    (Cvalue.V.Top_Param.O.singleton hidden_base)
  in
  let well_loc =
    Locations.make_loc
      (Location_Bits.inject hidden_base Ival.zero)
      (Int_Base.inject size)
  in
  let with_alarms = CilE.warn_none_mode in
  let state =
    if full
    then Cvalue.Model.add_binding ~with_alarms ~exact:true state well_loc well
    else state
  in
  Cvalue.Model.add_binding ~with_alarms ~exact:true state loc well

let create_hidden_base ~valid ~hidden_var_name ~name_desc pointed_typ =
  let hidden_var =
    Cil.makeGlobalVar ~generated:false ~logic:true hidden_var_name pointed_typ
  in
  Library_functions.register_new_var hidden_var pointed_typ;
  hidden_var.vdescr <- Some name_desc;
  let validity =
    if valid
    then begin
        match Base.validity_from_type hidden_var with
        | Base.Known (a,b)
            when not (Value_parameters.AllocatedContextValid.get ()) ->
            Base.Unknown (a,b)
        | (Base.All |  Base.Unknown _ | Base.Known _)  as s -> s
        | Base.Periodic _ -> assert false
      end
    else Base.Known(Int.one, Int.zero) (* Base.invalid *)
  in
  let hidden_base = Base.create_logic hidden_var validity in
  hidden_base

(** [initialize_var_using_type varinfo state] uses the type of [varinfo]
    to create an initial value in [state]. *)
let initialize_var_using_type varinfo state =
  let with_alarms = CilE.warn_none_mode in
  Cil.CurrentLoc.set varinfo.vdecl;
  let rec add_offsetmap depth v name_desc name typ offset_orig typ_orig state =
    let typ = Cil.unrollType typ in
    let loc = loc_of_typoffset v typ_orig offset_orig in
    let bind_entire_loc ?(state=state) v = (* Shortcut *)
      Cvalue.Model.add_binding ~with_alarms ~exact:true state loc v
    in
      match typ with
      | TInt _ | TEnum (_, _)->
          bind_entire_loc Cvalue.V.top_int
            
      | TFloat ((FDouble | FLongDouble as fkind), _) ->
          if fkind = FLongDouble
          then
            Value_parameters.warning ~once:true
              "Warning: unsupported long double treated as double";
          bind_entire_loc Cvalue.V.top_float
      | TFloat (FFloat, _) ->
          bind_entire_loc Cvalue.V.top_single_precision_float

      | TFun _ ->
          bind_entire_loc (Cvalue.V.top_leaf_origin ())

      | TPtr (typ, _) as full_typ
          when depth <= Value_parameters.AutomaticContextMaxDepth.get () ->
          let attr = Cil.typeAttr full_typ in
          let context_max_width =
            Value_parameters.AutomaticContextMaxWidth.get ()
          in
          if not (Cil.isVoidType typ) && not (Cil.isFunctionType typ) then
            let i =
              match Cil.findAttribute "arraylen" attr with
              | [AInt i] -> i
              | _ -> My_bigint.of_int context_max_width
            in
            let arr_pointed_typ =
              TArray(typ,
                     Some (Cil.kinteger64 ~loc:varinfo.vdecl IULong i),
                     Cil.empty_size_cache (),
                     [])
            in
            let hidden_var_name =
              Cabs2cil.fresh_global ("S_" ^ name)
            in
            let name_desc = "*"^name_desc in
            let hidden_base =
              create_hidden_base
                ~valid:true
                ~hidden_var_name
                ~name_desc
                arr_pointed_typ
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
          else
            let hidden_var_name = Cabs2cil.fresh_global ("S_" ^ name) in
            let name_desc = "*"^name_desc in
            let hidden_base =
              create_hidden_base
                ~valid:false
                ~hidden_var_name
                ~name_desc
                typ
            in
            make_well ~full:false hidden_base state loc

      | TArray (typ, len, _, _) ->
          begin try
            let size = Cil.lenOfArray len in
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
                add_offsetmap depth v name_desc name typ offset typ_orig !state;
              locs := loc_of_typoffset v typ_orig offset :: !locs;
            done;
            if max_precise_size < size then begin
              (* Some elements remain to be initialized *)
              let offsm_of_loc loc = (* This rereads one of the first cells*)
                Extlib.the
                  (Cvalue.Model.copy_offsetmap ~with_alarms loc !state)
              in
              let last_loc, locs = match !locs with
                | [] -> assert false (* AutomaticContextMaxWidth is at least 1*)
                | l :: ll -> l, ll
              in
              (* Join of the contents of the first elements *)
              let offsm_joined = List.fold_left
                (fun offsm loc ->
                   let offsm' = offsm_of_loc loc in
                   Cvalue.V_Offsetmap_ext.join offsm offsm'
                ) (offsm_of_loc last_loc) locs
              in
              let nb_fields = Cvalue.V_Offsetmap_ext.fold_internal
                (fun _itv _ -> succ) offsm_joined 0
              in
              let size_elt = Int.of_int (Cil.bitsSizeOf typ) in
              if nb_fields = 1 then
                (* offsm_joined is very regular (typically Top_int, or some
                  pointers). We read its contents and copy it everywhere else.
                  The periodicity of the contents may be smaller than the size
                   of a cell; take this into account. *)
                let offset, modu, v =
                  Extlib.the (Cvalue.V_Offsetmap_ext.fold_internal
                                (fun _itv v _ -> Some v) offsm_joined None)
                in
                assert (Int.equal offset Int.zero);
                let offsm_repeat = V_Offsetmap_ext.create_initial v modu in
                let loc = Location_Bits.location_shift
                  (Ival.inject_singleton size_elt) last_loc.loc;
                in
                let ncells = size - max_precise_size in
                (* paste [size - max_precise_size] elements, starting from
                   the last location initialized + 1 *)
                state :=
                  Cvalue.Model.paste_offsetmap ~with_alarms
                    ~from:offsm_repeat
                    ~dst_loc:loc
                    ~start:Int.zero
                    ~size:(Int.mul size_elt (Int.of_int ncells))
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
                loc := Location_Bits.location_shift
                  (Ival.inject_singleton size_elt) !loc;
                state :=
                  Cvalue.Model.paste_offsetmap ~with_alarms
                    ~from:offsm_joined
                    ~dst_loc:!loc
                    ~start:Int.zero
                    ~size:size_elt
                    ~exact:true
                    !state
                done);
            end;
            !state
          with Cil.LenOfArray ->
            Value_parameters.result ~once:true ~current:true
              "could not find a size for array";
            state
          end

      | TComp ({cstruct=true;} as compinfo, _, _) -> (* Struct *)
          let treat_field (next_offset,state) field =
            let new_offset = Field (field, NoOffset) in
            let offset = Cil.addOffset new_offset offset_orig in
            let field_offset,field_width = Cil.bitsOffset typ_orig offset in
            let state =
              if field_offset>next_offset then (* padding bits need filling*)
                let loc = make_loc
                  (Location_Bits.inject v (Ival.of_int next_offset))
                  (Int_Base.inject (Int.of_int (field_offset-next_offset)))
                in
                Cvalue.Model.add_binding_not_initialized state loc
              else state
            in
            field_offset+field_width,
            add_offsetmap
              depth
              v
              (name_desc ^ "." ^ field.fname)
              (field.fname^"_"^name)
              field.ftype
              offset
              typ_orig
              state
          in
          begin try
            let boff,bwidth = Cil.bitsOffset typ_orig offset_orig in
            let last_offset,state = List.fold_left
              treat_field
              (boff,state)
              compinfo.cfields
            in
            if last_offset<(boff+bwidth) then (* padding at end of struct*)
              let loc = make_loc
                (Location_Bits.inject v (Ival.of_int last_offset))
                (Int_Base.inject (Int.of_int (boff+bwidth-last_offset)))
              in
              Cvalue.Model.add_binding_not_initialized state loc
            else state
          with Cil.SizeOfError _ ->
            Value_parameters.warning ~once:true ~current:true
              "size of struct '%s' cannot be computed@." compinfo.corig_name;
            state
          end

      | TComp ({cstruct=false}, _, _) when Cil.is_fully_arithmetic typ ->
          (* Union of arithmetic types *)
          bind_entire_loc Cvalue.V.top_int

      | TPtr _ when Value_parameters.AllocatedContextValid.get () ->
          (* deep pointers map to NULL in this case *)
          bind_entire_loc Cvalue.V.singleton_zero

      | TBuiltin_va_list _ | TComp _ | TVoid _  | TPtr  _ ->
          (* variable arguments or union with non-arithmetic type
             or deep pointers *)

          (* first create a new varid and offsetmap for the
             "hidden location" *)
          let hidden_var_name =
            Cabs2cil.fresh_global ("WELL_"^name)
          in
          let hidden_var =
            Cil.makeGlobalVar ~logic:true hidden_var_name Cil.charType
          in
          hidden_var.vdescr <- Some (name_desc^"_WELL");
          let hidden_base =
            Base.create_logic
              hidden_var
              (Base.Known (Int.zero,Bit_utils.max_bit_address ()))
          in
          make_well ~full:true hidden_base state loc
      | TNamed (_, _)  -> assert false
  in
  add_offsetmap
    0
    (Base.create_varinfo varinfo)
    varinfo.vname varinfo.vname varinfo.vtype NoOffset varinfo.vtype state

let initialized_padding () =
  Value_parameters.InitializedPaddingGlobals.get ()

(* initialize the padding needing for type [typ], assuming that
   [last_bitsoffset] bits have been initialized. The padding is added
   starting from [lval+abs_offset bits] *)
let init_trailing_padding state ~last_bitsoffset ~abs_offset typ lval =
  try
    let size_to_add = Cil.bitsSizeOf typ - last_bitsoffset in
    let offset = Ival.inject_singleton (Int.of_int abs_offset) in
    assert (size_to_add >= 0);
    if size_to_add <> 0 then
      let loc =
        match lval with
          | Var vinfo, _  ->
            let base = Base.create_varinfo vinfo in
            let size_to_add = Int.of_int size_to_add in
            let offset, size =
              match Base.validity base with
                | Base.Periodic (mn, _mx, p) when Int.ge size_to_add p ->
                    Ival.inject_singleton mn, p
                | _ -> offset, size_to_add
            in
            let loc = Location_Bits.inject base offset in
            make_loc loc (Int_Base.inject size)
          | _ -> assert false
      in
      if initialized_padding ()
      then
	let v =
          if typeHasAttribute "volatile" typ
          then V.top_int
          else V.singleton_zero
	in
        Cvalue.Model.add_binding ~with_alarms:CilE.warn_none_mode
          ~exact:true state loc v
      else
        Cvalue.Model.add_binding_not_initialized state loc
    else state
  with Cil.SizeOfError _ ->
    Value_parameters.result ~once:true ~current:true
      "cannot provide a default initializer: size is unknown";
    state

(* Evaluation of a [SingleInit] in Cil parlance *)
let eval_single_initializer state lval exp =
  let with_alarms = CilE.warn_none_mode in
  let typ_lval = Cil.typeOfLval lval in
  (* Eval in Top state. We do not want the location to depend on other globals*)
  let loc = Eval_exprs.lval_to_loc ~with_alarms Cvalue.Model.top lval in
  if not (cardinal_zero_or_one loc) then
    Value_parameters.fatal ~current:true
     "In global initialisation, the location can not be represented. Aborting.";
  let value =
    Eval_exprs.eval_expr ~with_alarms:(warn_all_quiet_mode ()) state exp in
  let v =
    if typeHasAttribute "volatile" typ_lval
    then V.top_int
    else if Int_Base.equal loc.Locations.size
              (Int_Base.inject (Int.of_int ((Cil.bitsSizeOf (Cil.typeOf exp)))))
    then value
    else (* bit-field *)
      let size = Int_Base.project loc.Locations.size in
      Eval_exprs.cast_lval_bitfield lval size value
  in
  Cvalue.Model.add_binding ~with_alarms ~exact:true state loc v


let rec eval_initializer state lval init =
  match init with
    | SingleInit exp -> eval_single_initializer state lval exp

    | CompoundInit (base_typ, l) ->
      if typeHasAttribute "volatile" base_typ
      then state (* initializer is not useful *)
      else
        let last_bitsoffset, state =
          Cil.foldLeftCompound
            ~implicit:(not (initialized_padding ()))
            ~doinit:
            (fun off init typ (acc, state) ->
              let o,w = Cil.bitsOffset base_typ off in
              (* Format.printf "acc:%d o:%d w:%d@." acc o w; *)
              let state =
                if acc<o
                then begin (* uninitialize the padding bits *)
                  let vi, (base_off,_) =
                    (match lval with
                      | Var vinfo, abs_offset ->
                        vinfo,
                        (Cil.bitsOffset vinfo.vtype abs_offset)
                      | _ ->
                        Value_parameters.fatal "Whacky initializer?")
                  in
                  let loc_bits =
                    Location_Bits.inject
                      (Base.create_varinfo vi)
                      (Ival.inject_singleton (Int.of_int (base_off+acc)))
                  in
                  let loc_size = Int_Base.inject (Int.of_int (o-acc)) in
                  let loc = make_loc loc_bits loc_size in
                  (* Format.printf "loc:%a@." Locations.pretty loc; *)
                  Cvalue.Model.add_binding_not_initialized state loc
                end
                else (assert (acc=o); state)
              in
              if typeHasAttribute "volatile" typ then
                warning_once_current
                  "global initialization of volatile %s ignored"
                  (match off with
                    | Field _ -> "field"
                    | Index _ -> "array element"
                    | NoOffset -> "element");
              o+w,
              eval_initializer state (Cil.addOffsetLval off lval) init
            )
            ~ct:base_typ
            ~initl:l
            ~acc:(0, state)
        in
        let base_off,_ =
          (match lval with
            | Var vinfo, abs_offset -> Cil.bitsOffset vinfo.vtype abs_offset
            | _ -> Value_parameters.fatal "Whacky initializer?")
        in
	if initialized_padding ()
	then
	  init_trailing_padding state ~last_bitsoffset
            ~abs_offset:(base_off+last_bitsoffset)
            base_typ
            lval
        else state

(* Special initializers. Only lval with attributes 'const' are initialized *)
let rec eval_const_initializer state lval init =
  match init with
    | SingleInit exp ->
      let typ_lval = Cil.typeOfLval lval in
      let attrs = Cil.typeAttrs typ_lval in
      if Cil.hasAttribute "const" attrs &&
        not (Cil.hasAttribute "volatile" attrs)
      then
        eval_single_initializer state lval exp
      else state

    | CompoundInit (base_typ, l) ->
      if typeHasAttribute "volatile" base_typ ||
        not (Cil.typeHasAttributeDeep "const" base_typ)
      then state (* initializer is not useful *)
      else
        Cil.foldLeftCompound
          ~implicit:true
          ~doinit:
          (fun off init _typ state ->
            eval_const_initializer state (Cil.addOffsetLval off lval) init)
          ~ct:base_typ
          ~initl:l
          ~acc:state

(** Compute the initial values for globals and NULL *)
let initial_state_only_globals =
  let module S =
    State_builder.Option_ref
      (Cvalue.Model)
      (struct
        let name = "only_globals"
        let dependencies =
          [ Ast.self; Kernel.LibEntry.self; Kernel.MainFunction.self ]
      end)
  in
  function () ->
    let compute ()  =
      Value_parameters.debug ~level:2 "Computing globals values";
      let state = ref Cvalue.Model.empty_map in
      let update_state st' =
        if not (Db.Value.is_reachable st')
        then raise Initialization_failed
        else state := st'
      in
      Globals.Vars.iter_in_file_order
        (fun varinfo init ->
          if not varinfo.vlogic then begin
              Cil.CurrentLoc.set varinfo.vdecl;
              let volatile = typeHasAttribute "volatile" varinfo.vtype in
              match init.init, volatile with
              | None, _ | _, true -> (* Default to zero init *)
                  if volatile && init.init != None then
                    warning_once_current
                      "global initialization of volatile value ignored";
                  if varinfo.vstorage = Extern
                  then
                    (* Must not assume zero when the storage is extern. *)
                    update_state (initialize_var_using_type varinfo !state)
                  else if initialized_padding ()
		  then
		    update_state (
                      init_trailing_padding !state ~last_bitsoffset:0
                        ~abs_offset:0 varinfo.vtype (Var varinfo,NoOffset))
		  else
		    let typ = varinfo.vtype in
		    let loc = Cil_datatype.Location.unknown in
		    let zi = Cil.makeZeroInit ~loc typ in
		    update_state
                      (eval_initializer !state (Var varinfo,NoOffset) zi)
              | Some i, false ->
                  update_state
                    (eval_initializer !state (Var varinfo,NoOffset) i)
            end);

      (** Bind the declared range for NULL to uninitialized *)
      let min_valid = Base.min_valid_absolute_address () in
      let max_valid = Base.max_valid_absolute_address () in
      if Int.le min_valid max_valid
      then begin
          let loc_bits =
            Location_Bits.inject_ival (Ival.inject_singleton min_valid)
          in
          let loc_size =
            Int_Base.inject (Int.length min_valid max_valid)
          in
          if true (* TODO: command line option *)
          then
            update_state
              (Cvalue.Model.add_binding
                ~with_alarms:CilE.warn_none_mode
                ~exact:true
                !state
                (make_loc loc_bits loc_size)
                Cvalue.V.top_int)
          else
            update_state
              (Cvalue.Model.add_binding_not_initialized
                !state
                (make_loc loc_bits loc_size))
        end;
      let result = !state in
      result
    in
    S.memo
      (fun () -> 
         try compute ()
         with Initialization_failed -> Cvalue.Model.bottom)

module ContextfreeGlobals =
  State_builder.Option_ref
    (Cvalue.Model)
    (struct
      let name = "Value.Initial_state.ContextfreeGlobals"
      let dependencies =
        [ Ast.self; Kernel.LibEntry.self; Kernel.MainFunction.self ]
     end)

let () = Ast.add_monotonic_state ContextfreeGlobals.self

(** Initial state in [-lib-entry] mode *)
let initial_state_contextfree_only_globals () =
    let add_varinfo state vi =
      Cil.CurrentLoc.set vi.vdecl;
      let state = initialize_var_using_type vi state in
      (* We may do a second phase to initialize const fields again. In the
         first phase, they have been set to generic values *)
      if Cil.typeHasAttributeDeep "const" vi.vtype then
        let init = match (Globals.Vars.find vi).init with
          | None -> Cil.makeZeroInit ~loc:vi.vdecl vi.vtype
          | Some init -> init
        in
        eval_const_initializer state (Var vi, NoOffset) init
      else state
    in
    let treat_global state = function
      | GVar(vi,_,_) -> add_varinfo state vi
      | GVarDecl(_,vi,_) when not (Cil.isFunctionType vi.vtype) ->
          add_varinfo state vi
      | GType _ | GCompTag _ | GCompTagDecl _ | GEnumTag _ | GEnumTagDecl _
      | GVarDecl _ | GFun _ | GAsm _ | GPragma _ | GText _ | GAnnot _ -> state
    in
    let compute ()  =
      List.fold_left treat_global (initial_state_only_globals())
        (Ast.get ()).globals
    in
    ContextfreeGlobals.memo compute

let () =
  Db.Value.initial_state_only_globals :=
    (fun () ->
      if snd(Globals.entry_point ()) then
        initial_state_contextfree_only_globals ()
      else
        initial_state_only_globals ()
    );

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
