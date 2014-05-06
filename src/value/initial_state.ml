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

(** Creation of the initial state for Value *)

open Cil_types
open Abstract_interp
open Cvalue
open Locations
open Value_util

exception Initialization_failed

let typeHasAttribute attr typ = Cil.hasAttribute attr (Cil.typeAttrs typ)

let add_initialized state loc v =
  Cvalue.Model.add_initial_binding state loc (V_Or_Uninitialized.initialized v)
let add_unitialized state loc =
  Cvalue.Model.add_initial_binding state loc V_Or_Uninitialized.uninitialized


(* If [filled] is true, bind the contents of hidden_base to a well of itself.
   If not, do not bind it (typically for function pointers) *)
let make_well ~filled hidden_base state loc =
  let size = Bit_utils.max_bit_size () in
  let well =
    Cvalue.V.inject_top_origin Origin.Well (Base.Hptset.singleton hidden_base)
  in
  let well_loc =
    Locations.make_loc
      (Location_Bits.inject hidden_base Ival.zero)
      (Int_Base.inject size)
  in
  let state =
    if filled
    then add_initialized state well_loc well
    else state
  in
  add_initialized state loc well


let warn_unknown_size_aux pp v (messt, t) =
  Value_parameters.warning ~once:true ~current:true
    "@[during initialization@ of %a,@ size of@ type '%a'@ cannot be@ computed@ \
    (%s)@]" pp v Printer.pp_typ t messt

let warn_unknown_size =
  warn_unknown_size_aux
    (fun fmt v -> Format.fprintf fmt "variable '%a'" Printer.pp_varinfo v)

type validity_hidden_base =
  | Invalid (* Base is completely invalid *)
  | UnknownValidity (* Base is maybe invalid on its entire validity *)
  | KnownThenUnknownValidity of Integer.t (* Base is valid on i bits, then
                              maybe invalid on the remainder of its validity *)

let create_hidden_base ~valid ~hidden_var_name ~name_desc pointed_typ =
  let hidden_var =
    Cil.makeGlobalVar ~generated:false ~logic:true hidden_var_name pointed_typ
  in
  Library_functions.register_new_var hidden_var pointed_typ;
  hidden_var.vdescr <- Some name_desc;
  let validity =
    match valid with
    | Invalid -> Base.Invalid
    | _ ->
      (* Add a special case for void* pointers: we do not want to compute the
         size of void *)
      let validity = match Cil.unrollType pointed_typ with
        | TVoid _ -> Base.Unknown (Int.zero, None, Bit_utils.max_bit_address ())
        | _ -> Base.validity_from_type hidden_var
      in
      match validity with
        | Base.Known (a,b)
            when not (Value_parameters.AllocatedContextValid.get ()) ->
            (match valid with
               | KnownThenUnknownValidity size ->
                   let size = Integer.pred size in
                   assert (Integer.le size b);
                   Base.Unknown (a, Some size, b)
               | _ -> Base.Unknown (a, None, b)
            )
        | Base.Unknown _ | Base.Known _ | Base.Invalid as s -> s
        | Base.Periodic _ -> assert false
  in
  Base.register_memory_var hidden_var validity

(** [initialize_var_using_type varinfo state] uses the type of [varinfo]
    to create an initial value in [state]. *)
let initialize_var_using_type varinfo state =
  let with_alarms = CilE.warn_none_mode in
  Cil.CurrentLoc.set varinfo.vdecl;
  let rec add_offsetmap depth v name_desc name typ offset_orig typ_orig state =
    let typ = Cil.unrollType typ in
    let loc = loc_of_typoffset v typ_orig offset_orig in
    let bind_entire_loc ?(state=state) v = (* Shortcut *)
      add_initialized state loc v
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

      | TFun _ -> state

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
              | _ -> Integer.of_int context_max_width
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
          else
            let hidden_var_name = Cabs2cil.fresh_global ("S_" ^ name) in
            let name_desc = "*"^name_desc in
            let valid, filled =
              if Cil.isFunctionType typ then Invalid, false
              else UnknownValidity, true
            in   
            let hidden_base =
              create_hidden_base ~valid ~hidden_var_name ~name_desc typ
            in
            make_well ~filled hidden_base state loc

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
                add_offsetmap depth v name_desc name typ offset typ_orig !state;
	      let loc = loc_of_typoffset v typ_orig offset in
	      if Locations.loc_size loc = Int_Base.Top
	      then begin
		Value_parameters.warning "During initialization of variable %a (of type %a), an array of type %a of unknown size was encountered. It's impossible to represent this array without knowning the size of %a. Bailing out"
		  Base.pretty v
		  Printer.pp_typ typ_orig
		  Printer.pp_typ typ
		  Printer.pp_typ typ;
		raise Initialization_failed;
	      end;
              locs := loc :: !locs;
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
	      let last_offsm = offsm_of_loc last_loc in
              (* Join of the contents of the first elements *)
              let offsm_joined = 
		List.fold_left
                  (fun offsm loc ->
                    let offsm' = offsm_of_loc loc in
                    Cvalue.V_Offsetmap.join offsm offsm') 
		  last_offsm
		  locs
              in
              (* TODO: add Offsetmap.paste_repeated_slices to Offsetmap, and
                 replace everything below by a call to it. *)
              let nb_fields =
		  Cvalue.V_Offsetmap.fold 
                    (fun _itv _ -> succ)
		    offsm_joined 
		    0
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
                  Cvalue.Model.paste_offsetmap ~with_alarms
                    ~from:offsm_repeat
                    ~dst_loc:loc
                    ~start:Int.zero
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
          with
            | Cil.LenOfArray ->
                Value_parameters.result ~once:true ~current:true
                  "could not find a size for array";
                state (* TODOBY: use same strategy as for pointer *)
            | Cil.SizeOfError (s, t) ->
                warn_unknown_size varinfo (s, t);
                bind_entire_loc Cvalue.V.top_int;
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
                add_unitialized state loc
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
              add_unitialized state loc
            else state
          with Cil.SizeOfError (s, t) ->
            warn_unknown_size varinfo (s, t);
            bind_entire_loc Cvalue.V.top_int;
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
          let validity = Base.Known (Int.zero, Bit_utils.max_bit_address ()) in
          let hidden_base = Base.register_memory_var hidden_var validity in
          make_well ~filled:true hidden_base state loc
      | TNamed (_, _)  -> assert false
  in
  add_offsetmap
    0
    (Base.of_varinfo varinfo)
    varinfo.vname varinfo.vname varinfo.vtype NoOffset varinfo.vtype state


let init_var_zero vi state =
  let loc = Locations.loc_of_varinfo vi in
  let v =
    if typeHasAttribute "volatile" vi.vtype
    then V.top_int
    else V.singleton_zero
  in
  (try
     ignore (Cil.bitsSizeOf vi.vtype)
   with Cil.SizeOfError (s, t)->
     warn_unknown_size vi (s, t);
  );
  add_initialized state loc v


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
            let base = Base.of_varinfo vinfo in
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
        add_initialized state loc v
      else
        add_unitialized state loc
    else state
  with Cil.SizeOfError (s,t) ->
    warn_unknown_size_aux Printer.pp_lval lval  (s, t);
    state

(* Evaluation of a [SingleInit] in Cil parlance *)
let eval_single_initializer state lval exp =
  let with_alarms = CilE.warn_none_mode in
  (* Eval in Top state. We do not want the location to depend on other globals*)
  let _, loc, typ_lval =
    Eval_exprs.lval_to_loc_state ~with_alarms Cvalue.Model.top lval
  in
  if not (cardinal_zero_or_one loc) then
    Value_parameters.fatal ~current:true
     "In global initialisation, the location cannot be represented. Aborting.";
  let value =
    Eval_exprs.eval_expr ~with_alarms:(warn_all_quiet_mode ()) state exp
  in
  if Cvalue.V.equal value V.bottom then (
    Value_parameters.result ~source:(fst exp.eloc)
      "Evaluation of initializer '%a' failed@." Printer.pp_exp exp;
    raise Initialization_failed);
  let v =
    if typeHasAttribute "volatile" typ_lval
    then V.top_int
    else
      if Eval_op.is_bitfield typ_lval
      then Eval_op.cast_lval_bitfield typ_lval loc.Locations.size value
      else value
  in
  add_initialized state loc v


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
                      (Base.of_varinfo vi)
                      (Ival.inject_singleton (Int.of_int (base_off+acc)))
                  in
                  let loc_size = Int_Base.inject (Int.of_int (o-acc)) in
                  let loc = make_loc loc_bits loc_size in
                  (* Format.printf "loc:%a@." Locations.pretty loc; *)
                  add_unitialized state loc
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

(** Initial value for globals and NULL in no-lib-entry mode (everything
    initialized to 0). *)
let initial_state_only_globals =
  let module S =
    State_builder.Option_ref
      (Cvalue.Model)
      (struct
        let name = "Value.Initial_state.Not_context_free"
        let dependencies =
          [ Ast.self; Kernel.AbsoluteValidRange.self;
            Value_parameters.InitializedPaddingGlobals.self ]
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
                  else
                  if initialized_padding () then
		    update_state (init_var_zero varinfo !state)
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

      (** Bind the declared range for NULL to top int *)
      let min_valid = Base.min_valid_absolute_address () in
      let max_valid = Base.max_valid_absolute_address () in
      if Int.le min_valid max_valid
      then begin
        (* Bind everything between [0..max] to bottom. Offsetmaps cannot
           contain holes, which would happend if min > 0 holds. *)
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
        state := Cvalue.Model.add_base Base.null offsm !state
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
      open Value_parameters
      let dependencies =
        [ Ast.self; Kernel.AbsoluteValidRange.self;
          InitializedPaddingGlobals.self; AllocatedContextValid.self;
          AutomaticContextMaxWidth.self; AutomaticContextMaxDepth.self;
        ]
     end)

let () = Ast.add_monotonic_state ContextfreeGlobals.self

(** Initial state in [-lib-entry] mode *)
let initial_state_contextfree_only_globals () =
    let add_varinfo state vi =
      Cil.CurrentLoc.set vi.vdecl;
      let state = initialize_var_using_type vi state in
      (* We may do a second phase to initialize const fields again. In the
         first phase, they have been set to generic values *)
      if Cil.typeHasAttributeDeep "const" vi.vtype && not (vi.vstorage = Extern)
      then
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
