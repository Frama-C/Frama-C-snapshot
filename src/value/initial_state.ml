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

(** Creation of the initial state for Value *)

open Cil_types
open Cil
open Abstract_interp
open Cvalue
open Locations
open Bit_utils
open Value_util

exception Initialization_failed

let make_well hidden_base state loc =
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
  let state_with_well =
    Cvalue.Model.add_binding
      ~with_alarms:CilE.warn_none_mode
      ~exact:true
      state
      well_loc
      well
  in
  Cvalue.Model.add_binding
    ~with_alarms:CilE.warn_none_mode
    ~exact:true
    state_with_well
    loc
    well

let create_hidden_base
    validity_from_type ~hidden_var_name ~name_desc pointed_typ =
  let hidden_var =
    makeGlobalVar ~generated:false ~logic:true hidden_var_name pointed_typ
  in
  Globals.Vars.add_decl hidden_var;
  hidden_var.vdescr <- Some name_desc;
  let validity =
    if validity_from_type
    then begin
        match Base.validity_from_type hidden_var with
        | Base.Known (a,b)
            when not (Value_parameters.AllocatedContextValid.get ()) ->
            Base.Unknown (a,b)
        | (Base.All |  Base.Unknown _ | Base.Known _)  as s -> s
        | Base.Periodic _ -> assert false
      end
    else Base.Unknown (Int.zero,Bit_utils.max_bit_address ())
  in
  let hidden_base = Base.create_logic hidden_var validity
  in
  hidden_base

(** [initialize_var_using_type varinfo state] uses the type of [varinfo]
    to create an initial value in [state]. *)
let initialize_var_using_type varinfo state =
  CurrentLoc.set varinfo.vdecl;
  let initializing_formal = not varinfo.vglob in
  let force_initialize =
    initializing_formal ||
      (Cvalue.Model.has_been_initialized
            (Base.create_varinfo varinfo)
            state)
  in
  let rec add_offsetmap depth v name_desc name typ offset_orig typ_orig state =
    let typ = Cil.unrollType typ in
    let loc = loc_of_typoffset v typ_orig offset_orig in
    let rec treat_as_const typ =
      (hasAttribute "const" (typeAttrs typ)) ||
        ( match typ with
          TArray (typ, _,_,_) -> treat_as_const (Cil.unrollType typ)
        | _ -> false)
    in
    let must_initialize =
      force_initialize || (not (treat_as_const typ))
    in
    if not must_initialize
      (* if we do not have an initializer for this const, we generate
         a formal constant *)
    then state else
      match typ with
      | TInt _ | TEnum (_, _)->
          Cvalue.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            state
            loc
            Cvalue.V.top_int
      | TFloat ((FDouble | FLongDouble as fkind), _) ->
          if fkind = FLongDouble
          then
            Value_parameters.warning
              ~once:true
              "Warning: unsupported long double treated as double";
          Cvalue.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            state
            loc
            Cvalue.V.top_float
      | TFloat (FFloat, _) ->
            Cvalue.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            state
            loc
            Cvalue.V.top_single_precision_float
      | TFun _ ->
          Cvalue.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            state
            loc
            (Cvalue.V.top_leaf_origin ())
      | TPtr (typ, _) as full_typ
          when depth <= Value_parameters.AutomaticContextMaxDepth.get () ->
          let attr = typeAttr full_typ in
          let context_max_width =
            Value_parameters.AutomaticContextMaxWidth.get ()
          in
          if not (isVoidType typ) && not (isFunctionType typ) then
            let i =
              match findAttribute "arraylen" attr with
              | [AInt i] -> i
              | _ -> context_max_width
            in
            let pointed_typ =
              TArray(typ,
                     Some (integer ~loc:varinfo.vdecl i),
                     empty_size_cache (),
                     [])
            in
            let hidden_var_name =
              Cabs2cil.fresh_global ("S_" ^ name)
            in
            let name_desc = "*"^name_desc in
            let hidden_base =
              create_hidden_base
                true
                ~hidden_var_name
                ~name_desc
                pointed_typ
            in
            let state =
              add_offsetmap
                (depth + 1)
                hidden_base
                name_desc
                hidden_var_name
                pointed_typ
                NoOffset
                pointed_typ
                state
            in
            let value = Cvalue.V.inject hidden_base (Ival.zero)
            in
            let value =
              if Value_parameters.AllocatedContextValid.get ()
              then value
              else Cvalue.V.join Cvalue.V.singleton_zero value
            in
            Cvalue.Model.add_binding
              ~with_alarms:CilE.warn_none_mode
              ~exact:true
              state
              loc
              value
          else
            let hidden_var_name =
              Cabs2cil.fresh_global ("S_" ^ name)
            in
            let name_desc = "*"^name_desc in
            let hidden_base =
              create_hidden_base
                false
                ~hidden_var_name
                ~name_desc
                typ
            in
            make_well hidden_base state loc

      | TArray (typ, len, _, _) ->
          begin try
            let size = lenOfArray len in
            let state = ref state in
            let typ = Cil.unrollType typ in
            let treat_index (i : int) =
              let offset =
                addOffset
                  (Index (integer ~loc:varinfo.vdecl i, NoOffset))
                  offset_orig
              in
              let name = (string_of_int i) ^"_"^ name in
              let name_desc = name_desc ^ "[" ^ (string_of_int i) ^ "]" in
              let s =
                add_offsetmap depth v
                  name_desc name typ
                  offset typ_orig !state
              in
              state := s;
              let loc = loc_of_typoffset v typ_orig offset in
              let r_offsetmap =
                Cvalue.Model.copy_offsetmap
                  ~with_alarms:CilE.warn_none_mode
                  loc s
              in
              match r_offsetmap with
                Some r_offsetmap -> r_offsetmap
              | None -> assert false
            in
            let max_precise_size =
              Value_parameters.AutomaticContextMaxWidth.get ()
            in
            if size <= max_precise_size
            then
              for i = 0 to pred size do
                ignore (treat_index i)
              done
            else begin
(*              Format.printf "ST %a: size=%d max_precise_size=%d offset=%a@."
                  Base.pretty v
                  size
                  max_precise_size
                  (!d_offset) offset_orig ; *)
                let vv = ref None in
                for i = 0 to pred max_precise_size do
(*                Format.printf "IT %d@.%a@."
                    i
                    Cvalue.Model.pretty !state; *)
                  let r = treat_index i in
                  vv :=
                    Some
                      ( match !vv with
                        None -> r
                      | Some vv -> snd (Cvalue.V_Offsetmap.join r vv))
                done;
                ( match !vv with
                  None -> assert false
                | Some vv ->
(*                  Format.printf "EN %a@."
                      Cvalue.V_Offsetmap.pretty vv; *)
                    for i = max_precise_size to pred size do
                      let offset =
                        addOffset
                          (Index (integer ~loc:varinfo.vdecl i, NoOffset))
                          offset_orig
                      in
                      let loc = loc_of_typoffset v typ_orig offset in
                      let size =
                        try
                          Int_Base.project loc.size
                        with
                        | Int_Base.Error_Top
                        | Int_Base.Error_Bottom ->
                          assert false
                      in
                      state :=
                        Cvalue.Model.paste_offsetmap CilE.warn_none_mode
                          vv loc.loc Int.zero size true !state
                done);
              end;
            !state
          with LenOfArray ->
            Value_parameters.result ~once:true ~current:true
              "could not find a size for array";
            state
          end
      | TComp ({cstruct=true;} as compinfo, _, _) -> (* Struct *)
          let treat_field (next_offset,state) field =
            let new_offset = Field (field, NoOffset) in
            let offset =
              addOffset
                new_offset
                offset_orig
            in
            let field_offset,field_width = bitsOffset typ_orig offset in
            let state =
              if field_offset>next_offset then (* padding bits needs filling*)
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
            let boff,bwidth = bitsOffset typ_orig offset_orig in
            let last_offset,state= List.fold_left
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
          with Cil.SizeOfError _ -> state
          end
      | TComp ({cstruct=false}, _, _) when
          is_fully_arithmetic typ
          -> (* Union of arithmetic types *)
          Cvalue.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            state
            loc
            Cvalue.V.top_int

      | TPtr _ when Value_parameters.AllocatedContextValid.get () ->
          (* deep pointers map to NULL in this case *)
          Cvalue.Model.add_binding
            ~with_alarms:CilE.warn_none_mode
            ~exact:true
            state
            loc
            Cvalue.V.singleton_zero

      | TBuiltin_va_list _ | TComp _ | TVoid _  | TPtr  _ ->
          (* variable arguments or union with non-arithmetic type
             or deep pointers *)

          (* first create a new varid and offsetmap for the
             "hidden location" *)
          let hidden_var_name =
            Cabs2cil.fresh_global ("WELL_"^name)
          in
          let hidden_var =
            makeGlobalVar ~logic:true hidden_var_name charType
          in
          hidden_var.vdescr <- Some (name_desc^"_WELL");
          let hidden_base =
            Base.create_logic
              hidden_var
              (Base.Known (Int.zero,Bit_utils.max_bit_address ()))
          in
          make_well  hidden_base state loc
      | TNamed (_, _)  -> assert false
  in
  add_offsetmap
    0
    (Base.create_varinfo varinfo)
    varinfo.vname varinfo.vname varinfo.vtype NoOffset varinfo.vtype state

let initial_state_only_globals =
  let module S =
    State_builder.Option_ref
      (Cvalue.Model)
      (struct
        let name = "only_globals"
        let dependencies =
          [ Ast.self; Kernel.LibEntry.self; Kernel.MainFunction.self ]
        let kind = `Internal
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
      let complete_init ~last_bitsoffset ~abs_offset typ _l lval =
        (* process the non initialized bits defaulting to 0 *)
        begin try
            let size_to_add, offset =
              bitsSizeOf typ - last_bitsoffset,
              Ival.inject_singleton (Int.of_int abs_offset)
            in
            assert (size_to_add >= 0);
            if size_to_add <> 0 then
              let loc =
                match lval with
                | Var vinfo, _  ->
                    let base = Base.create_varinfo vinfo in
                    let size_to_add = (Int.of_int size_to_add) in
                    let offset, size =
                      match Base.validity base with
                        Base.Periodic (mn, _mx, p) when Int.ge size_to_add p ->
                              Ival.inject_singleton mn, p
                      | _ -> offset, size_to_add
                    in
                    let loc =
                      Location_Bits.inject base offset
                    in
                    let loc = make_loc loc (Int_Base.inject size) in
(*                  Format.printf "loc for final zeroes %a@."
                      Locations.pretty loc; *)
                    loc
                | _ ->
                  Value_parameters.error ~current:true
                    "Whacky initializer ? Please report.";
                  assert false
              in
              let v =
                if hasAttribute "volatile" (typeAttrs typ)
                then V.top_int
                else V.singleton_zero
              in
              update_state
                (Cvalue.Model.add_binding
                  ~with_alarms:CilE.warn_none_mode
                  ~exact:true
                  !state
                  loc
                  v)
          with Cil.SizeOfError _ ->
            Value_parameters.result ~once:true ~current:true
              "cannot provide a default initializer: size is unknown"
        end
      in
      let rec eval_init lval init =
        match init with
        | SingleInit exp ->
            let loc =
              Eval_exprs.lval_to_loc ~with_alarms:CilE.warn_none_mode
                Cvalue.Model.top lval
            in
(*          Format.printf "loc:%a state before:%a@."
              Locations.pretty loc
              Cvalue.Model.pretty !state;   *)
            let exact = cardinal_zero_or_one loc in
            assert
              (if exact then true
               else begin
                 Value_parameters.warning ~current:true
                   "In global initialisation, the location can not be \
represented. Aborting.";
                 false
               end);
            let value =
              Eval_exprs.eval_expr ~with_alarms:(warn_all_quiet_mode ())
                !state
                exp
            in
            let v =
              if hasAttribute "volatile" (typeAttrs (Cil.typeOfLval lval))
              then V.top_int
              else if not (Int_Base.equal
                              loc.Locations.size
                              (Int_Base.inject
                                  (Int.of_int ((bitsSizeOf (typeOf exp))))))
              then (* bit-field *)
                (* same sequence used for assignment to bit-fields in the code;
                   refactor *)
                  Cvalue.V.cast ~with_alarms:CilE.warn_none_mode
                    ~size:(Int_Base.project loc.Locations.size)
                    ~signed:(signof_typeof_lval lval)
                    value
                else value
            in
            update_state
              (Cvalue.Model.add_binding
                ~with_alarms:CilE.warn_none_mode ~exact
                !state loc v);
(*          Format.printf "state after:%a@."
              Cvalue.Model.pretty !state;  *)

        | CompoundInit (base_typ, l) ->
            if not (hasAttribute "volatile" (typeAttrs base_typ))
            then
              let last_bitsoffset =
                foldLeftCompound
                  ~implicit:false
                  ~doinit:
                  (fun off init typ (acc:int) ->
                    let o,w = bitsOffset base_typ off in
                (*    Format.printf "acc:%d o:%d w:%d@." acc o w; *)
                    if acc<o
                    then begin (* uninitialize the padding bits *)
                        let vi, (base_off,_) =
                          (match lval with
                          | Var vinfo, abs_offset ->
                              vinfo,
                              (bitsOffset vinfo.vtype abs_offset)
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
(*                      Format.printf "loc:%a@." Locations.pretty loc; *)
                        update_state
                          (Cvalue.Model.add_binding_not_initialized
                            !state
                            loc)
                      end
                    else assert (acc=o);
                    if hasAttribute "volatile" (typeAttrs typ) then
                      warning_once_current
                        "global initialization of volatile value ignored"
                    else
                      eval_init (addOffsetLval off lval) init;
                    o+w)
                  ~ct:base_typ
                  ~initl:l
                  ~acc:0
              in
              let base_off,_ =
                (match lval with
                | Var vinfo, abs_offset ->
                              bitsOffset vinfo.vtype abs_offset
                | _ ->
                    Value_parameters.fatal "Whacky initializer?")
              in
          (*    Format.printf "last_bitsoffset:%d base_off:%d@\nstate after:%a@."
                last_bitsoffset
                base_off
                Cvalue.Model.pretty !state;  *)
              complete_init ~last_bitsoffset
                ~abs_offset:(base_off+last_bitsoffset)
                base_typ
                l
                lval
            else ()
      in
      Globals.Vars.iter_in_file_order
        (fun varinfo init ->
          if not varinfo.vlogic then begin
              CurrentLoc.set varinfo.vdecl;
              match init.init with
              | None -> (* Default to zero init *)
                  if varinfo.vstorage = Extern
                  then
                    (* Must not assume zero when the storage is extern. *)
                    update_state (initialize_var_using_type varinfo !state)
                  else
                    complete_init ~last_bitsoffset:0 ~abs_offset:0
                      varinfo.vtype [] (Var varinfo,NoOffset)
              | Some i ->
                  eval_init (Var varinfo,NoOffset) i
            end);

      (** Bind the declared range for NULL to uninitialized *)
      let min_valid = Base.min_valid_absolute_address () in
      let max_valid = Base.max_valid_absolute_address () in
      if Int.le min_valid max_valid
      then begin
          let loc_bits = 
	    Location_Bits.inject_ival
              (Ival.inject_singleton min_valid)
          in
          let loc_size =
            Int_Base.inject
              (Int.length min_valid max_valid)
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

(** Compute only once the initial values for globals and NULL *)
let initial_state_contextfree_only_globals =
  let module S =
    State_builder.Option_ref
      (Cvalue.Model)
      (struct
         let name = "contextfree_only_globals"
         let dependencies =
           [ Ast.self; Kernel.LibEntry.self; Kernel.MainFunction.self ]
         let kind = `Internal
       end)
  in
  function () ->
    let add_varinfo state varinfo =
      CurrentLoc.set varinfo.vdecl;
      initialize_var_using_type varinfo state
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
    S.memo compute

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
