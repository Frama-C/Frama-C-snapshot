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

open Cil_types
module FC_file = File
open Cil_datatype

exception Cannot_expand
exception Cannot_change

(* Build the term [p+i], assuming that [p] has pointer type *)
let plus_pi ~loc p i =
  if Integer.(equal zero i) then
    p
  else
    Cil.mkBinOp ~loc PlusPI p (Cil.kinteger64 ~loc i)

(** This visitor also performs a deep copy. *)
class propagate project fnames ~cast_intro = object(self)
  inherit Visitor.frama_c_copy project

  (* Variables which have already been declared earlier in the list of
     globals. Varinfos of the old project. *)
  val mutable known_globals = Varinfo.Set.empty

  (* Variables whose declaration must be put before the global we are visiting.
     Reset before each global. Varinfos of the _new_ project. *)
  val mutable must_add_decl = Varinfo.Set.empty

  method! vstmt_aux s=
    (* Do not propagate on 'return' statements: one invariant of the AST is
       that they must be of the form 'return v;' where 'v' is a variable *)
    match s.skind with
    | Return _ -> Cil.JustCopy
    | _ -> Cil.DoChildren

  method! vfunc fundec =
    if Cil_datatype.Fundec.Set.is_empty fnames || 
       Cil_datatype.Fundec.Set.mem fundec fnames
    then begin
      PropagationParameters.feedback
        ~level:2
        "propagated constant in function %s"
        (fundec.svar.vname);
      Cil.DoChildren
    end else Cil.JustCopy

  method private add_decl_non_source_var vi =
    PropagationParameters.debug ~level:2
      "Need to declare %a earlier" Printer.pp_varinfo vi;
    let vi' =
      Visitor.visitFramacVarDecl (self :> Visitor.frama_c_visitor) vi
    in
    must_add_decl <- Varinfo.Set.add vi' must_add_decl;
    known_globals <- Varinfo.Set.add vi known_globals;
    if Cil.isFunctionType vi.vtype then begin
      let kf = Globals.Functions.get vi in
      let new_kf = Cil.memo_kernel_function self#behavior kf in
      Queue.add (fun () -> Globals.Functions.register new_kf)
        self#get_filling_actions;
    end

  (* introduce a new cast from [oldt] to [newt] or do not expand [e] *)
  method private add_cast ~ignore_const_cast ~oldt ~newt e =
    (* strip the superfleous 'const' attribute (see bts #1787) on
       pointed values. *)
    let oldt, newt =
      if ignore_const_cast then
        match Cil.unrollType oldt, Cil.unrollType newt with
        | TPtr(typ, attrs), TPtr(typ', attrs') ->
          let drop_const ty = Cil.typeRemoveAttributes ["const"] ty in
          TPtr(drop_const typ, attrs), TPtr(drop_const typ', attrs')
        | _ -> oldt, newt
      else
        oldt, newt
    in
    let exp = Cil.mkCastT e oldt newt in
    if cast_intro then
      exp
    else match exp.enode with
    | CastE _ ->
      if exp == e (* older cast, no new cast added *) then
        exp
      else begin
        (* without [cast_intro], introducing such a cast is not
           allowed: do not expand [e] *)
        PropagationParameters.debug
          ~level:2
          "Need a cast introduction (force using -scf-allow-cast option)";
        raise Cannot_expand
      end
    | _ ->
      (* remember the change done by [mkCastT] (if any).
         note that [mkCastT] make some modifications, even if it
         does not introduce a new cast. *)
      exp

  (* Make sure that [expr] is in the original project. *)
  method private propagated expr ~ignore_const_cast =
    PropagationParameters.debug ~level:2
      "Replacing %s%a?" 
      (if ignore_const_cast then "(without const* cast) " else "")
      Printer.pp_exp expr;
    try
      let loc = expr.eloc in
      let typ = Cil.typeOf expr in
      let typ_e = Cil.unrollType typ in
      begin match typ_e with
      | (TInt _
            | TFloat _
            | TPtr _
            | TEnum _) -> ()
      | _ -> raise Cannot_expand
      end;
      let ki = match self#current_stmt with
        | None -> raise Cannot_change
        | Some s -> Kstmt s
      in
      let evaled = !Db.Value.access_expr ki expr in
      let k,m = Cvalue.V.find_lonely_binding evaled in
      let can_replace vi =
        (* can replace the current expr by [vi] iff (1) it is a source var, or
           expansion of non-source var is allowed. *)
        (vi.vsource || PropagationParameters.ExpandLogicContext.get ())
        &&
          (* (2) [vi] is bound in this function *)
          (vi.vglob ||
             Extlib.may_map
             (Kernel_function.is_formal_or_local vi) ~dft:false
             self#current_kf)
      in
      let change_to = match k with
        | Base.Var(vi,_)
        | Base.Initialized_Var (vi,_) when can_replace vi ->
          if vi.vglob && not (Varinfo.Set.mem vi known_globals) then
            self#add_decl_non_source_var vi;
          PropagationParameters.debug
            "Trying replacing %a from a pointer value {&%a + %a}"
            Printer.pp_exp expr Base.pretty k Ival.pretty m;
          let offset = Ival.project_int m in (* these are bytes *)
          let expr' =
            try
              if not (Cil.isPointerType typ_e) then
                raise Bit_utils.NoMatchingOffset;
              let typ_pointed = Cil.unrollType (Cil.typeOf_pointed typ_e) in
              if Cil.isVoidType typ_pointed then
                raise Bit_utils.NoMatchingOffset;
              let offset = Integer.mul offset Integer.eight in
              let m = Bit_utils.MatchType typ_pointed in
              let off, _ = Bit_utils.(find_offset vi.vtype ~offset m) in
              Cil.mkAddrOrStartOf ~loc (Var vi, off)
            with Bit_utils.NoMatchingOffset ->
              (* Build [((char* )&t[idx])+rem] when vi is an array, or
                 [(char* )(&vi+idx)+rem] otherwise. Automatically simplify
                 when [idx] or [rem] is zero. *)
              let array, idx, rem =
                let array, sizeof_pointed =
                  let array = Cil.isArrayType vi.vtype in
                  let size = if array
                    then Bit_utils.osizeof_pointed vi.vtype
                    else Bit_utils.osizeof vi.vtype
                  in
                  array, Int_Base.project size
                in
                array,
                (Integer.pos_div offset sizeof_pointed),
                (Integer.pos_rem offset sizeof_pointed)
              in
              let expr' =
                if array then
                  let off_idx =
                    if Integer.is_zero idx
                    then NoOffset
                    else Index (Cil.kinteger64 ~loc idx, NoOffset)
                  in
                  Cil.mkAddrOrStartOf ~loc (Var vi, off_idx)
                else
                  let start = Cil.mkAddrOrStartOf ~loc (Var vi, NoOffset) in
                  plus_pi ~loc start idx
              in
              if Integer.is_zero rem then expr'
              else
                plus_pi ~loc
                  (self#add_cast
                     ~ignore_const_cast:false
                     ~oldt:(Cil.typeOf expr')
                     ~newt:Cil.charPtrType
                     expr')
                  rem
          in
          (* preserve typing: propagating constant could change the type
             of the expression. We have to put back the original type. *)
          self#add_cast
            ~ignore_const_cast
            ~oldt:(Cil.typeOf expr')
            ~newt:typ
            expr'

        | Base.Null ->
	  let const_integer m ikind =
            try
              let v = Ival.project_int m in
              if not (Cil.fitsInInt ikind v) then
                PropagationParameters.error "Constant found by Value (%a) \
                    does not fit inside type %a. Please report"
                  Abstract_interp.Int.pretty v
                  Printer.pp_typ typ;
              Cil.kinteger64 ~loc ~kind:ikind v
            with Ival.Not_Singleton_Int -> raise Cannot_expand
	  and const_float m fkind =
            try
	      let v = Ival.project_float m in
	      let f1,f2 =  Fval.min_and_max v in
	      if not (Fval.F.equal f1 f2) then raise Cannot_expand ;
	      let f = Fval.F.to_float f1 in
	      Cil.kfloat ~loc:expr.eloc fkind f
	    with Ival.Nan_or_infinite -> raise Cannot_expand
	  in
          (match typ_e with
          | TFloat (fkind, _) -> const_float m fkind
          | TInt (ikind, _) | TEnum ({ ekind = ikind}, _) ->
            const_integer m ikind
          | _ -> raise Cannot_expand)

        | Base.String _ | Base.Var _ | Base.Initialized_Var _
        | Base.CLogic_Var _ -> raise Cannot_change
      in  
      PropagationParameters.debug "Replacing %a with %a"
	Printer.pp_exp expr
        Printer.pp_exp change_to;
      Some change_to
    with 
    | Cannot_change -> None
    | Not_found | Cannot_expand | Cil.Not_representable 
    | Int_Base.Error_Top as e ->
      PropagationParameters.debug "Replacement failed %s"
        (Printexc.to_string e);
      None

  method! vexpr expr =
    (* nothing is done for [expr] already being a constant *)
    match expr.enode with
    | Const (_) -> Cil.DoChildren
    | _ -> begin
    (* Start by trying to constant-propagate all of [expr]. Casts are allowed
       only if -scf-allow-cast is set *)
    match self#propagated expr ~ignore_const_cast:false with
    | Some expr' -> Cil.ChangeDoChildrenPost (expr', fun x -> x)
    | None -> begin
      (* Global constant propagation of [expr] failed. We try a special
         const-folding, AND simplify the sub-expressions in all cases *)
      match expr.enode with
      | Lval (Mem exp_mem, off) -> begin
        (* [expr] is a Mem. Try to see if we can propagate [exp_mem] into
           something simpler, because the result will be of the form
           [Var _, offs'], which can be simplified under a [Mem]. This time,
           we ignore const-related casts when simplifying [exp_mem], because
           they will disappear when the l-value is dereferenced. *)
        match self#propagated exp_mem ~ignore_const_cast:true with
        | Some exp_mem' ->
          let lv = Cil.new_exp expr.eloc (Lval (Cil.mkMem exp_mem' off)) in
          Cil.ChangeDoChildrenPost (lv, fun x -> x)
        | None -> Cil.DoChildren
      end
      | _ -> Cil.DoChildren
    end
    end

  method! vvdec v =
    if v.vglob then begin
      known_globals <- Varinfo.Set.add v known_globals;
    end;
    Cil.DoChildren

  method! vglob_aux g =
    must_add_decl <- Varinfo.Set.empty;
    (* Check if [g] has already been declared earlier, due to being used in
       some earlier values. If so, we will skip [g]. We do this check now and
       not in [add_decls], because [self#vvdec] will mark g as known. *)
    let g_is_known = match g with
      | GVarDecl (vi, _) | GFunDecl (_, vi, _) -> Varinfo.Set.mem vi known_globals
      | _ -> false
    in
    let add_decls l =
      (* Do not re-add a declaration for g if it is known. *)
      let l = if g_is_known then [] else l in
      (* Add declarations for the globals that are referenced in g's propagated
         value. *)
      Varinfo.Set.fold
        (fun vi l ->
          PropagationParameters.feedback ~level:2
            "Adding declaration of global %a" Printer.pp_varinfo vi;
          let g' =
            if Cil.isFunctionType vi.vtype
            then GFunDecl(Cil.empty_funspec(), vi, vi.vdecl)
            else GVarDecl(vi, vi.vdecl)
          in
          g' ::l)
        must_add_decl l
    in
    Cil.DoChildrenPost add_decls

  method! vlval lv =
    let simplify (host,offs as lv) = match host with
      | Mem e -> Cil.mkMem e offs (* canonize in case the propagation
                                     simplified [lv] *)
      | Var _ -> lv
    in
    Cil.ChangeDoChildrenPost(lv, simplify)

end

module Result_pair =
  Datatype.Pair_with_collections(Cil_datatype.Fundec.Set)(Datatype.Bool)
    (struct let module_name = "Constant_propagation.Register.Result_pair.t" end)
module Result =
  State_builder.Hashtbl
    (Datatype.Hashtbl
       (Result_pair.Hashtbl)
       (Result_pair)
       (struct let module_name = "Semantical constant propagation" end))
    (Project.Datatype)
    (struct
       let size = 7
       let name = "Semantical constant propagation"
       let dependencies =
         [ Db.Value.self; 
           PropagationParameters.CastIntro.self; 
           PropagationParameters.Project_name.self ]
     end)

let selection_command_line_option =
  State_selection.singleton PropagationParameters.SemanticConstFolding.self

let journalized_get =
  let get fnames cast_intro =
    Result.memo
      (fun _ ->
         !Db.Value.compute ();
         let fresh_project =
           FC_file.create_project_from_visitor
             (PropagationParameters.Project_name.get ())
             (fun prj -> new propagate prj fnames cast_intro)
         in
         let ctx = Parameter_state.get_selection_context () in
         let ctx = State_selection.diff ctx selection_command_line_option in
         Project.copy ~selection:ctx fresh_project;
         fresh_project)
      (fnames, cast_intro)
  in
  Journal.register
    "!Db.Constant_Propagation.get"
    (Datatype.func2
       Cil_datatype.Fundec.Set.ty
       ~label2:("cast_intro",None)
       Datatype.bool
       Project.ty)
    get

(* add labels *)
let get fnames ~cast_intro = journalized_get fnames cast_intro

(** Constant Propagation *)

let compute () =
  PropagationParameters.feedback "beginning constant propagation";
  let fnames = PropagationParameters.SemanticConstFold.get () in
  let cast_intro = PropagationParameters.CastIntro.get () in
  let propagated = get fnames cast_intro in
  if PropagationParameters.SemanticConstFolding.get () then
    FC_file.pretty_ast ~prj:propagated ();
  let project_name = Project.get_unique_name propagated in
  PropagationParameters.feedback  "@[constant propagation done%t@]"
    (fun fmt ->
      if project_name <> PropagationParameters.Project_name.get () then
        Format.fprintf fmt ",@ result is in project@ `%s`" project_name)

let main () =
  let force_semantic_folding =
    PropagationParameters.SemanticConstFolding.get ()
    || not (Cil_datatype.Fundec.Set.is_empty
              (PropagationParameters.SemanticConstFold.get ()))
  in
  (* must called the function stored in [Db] for journalisation purpose *)
  if force_semantic_folding then !Db.Constant_Propagation.compute ()

let () =
  Db.Main.extend main;
  Db.register Db.Journalization_not_required Db.Constant_Propagation.get get;
  ignore
    (Db.register_compute
       "Constant_Propagation.compute"
       [ PropagationParameters.SemanticConstFold.self;
         PropagationParameters.SemanticConstFolding.self;
         Result.self ]
       Db.Constant_Propagation.compute
       compute)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
