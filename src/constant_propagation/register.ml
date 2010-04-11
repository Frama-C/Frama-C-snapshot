(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

open Cil
open Cil_types
open Db
exception Cannot_expand

(** This visitor also performs a deep copy. *)
class propagate project fnames ~cast_intro = object(self)
  inherit Visitor.frama_c_copy project

  val mutable operate = false

  val mutable known_globals = Cilutil.VarinfoSet.empty

  val mutable must_add_decl = Cilutil.VarinfoSet.empty

  method private on_current_stmt nothing f =
    match self#current_stmt with
    | None | Some ({ skind = Return _}) -> nothing
    | Some _ when not operate -> nothing
    | Some stmt -> f (Kstmt stmt)

  method vfunc fundec =
    let name = fundec.svar.vname in
    operate <-
      Cilutil.StringSet.is_empty fnames || Cilutil.StringSet.mem name fnames;
    if operate then
      PropagationParameters.feedback
	~level:2
	"propagated constant in function %s"
        (fundec.svar.vname);
    DoChildren

  method vexpr expr =
    self#on_current_stmt
      DoChildren
      (fun ki ->
         PropagationParameters.debug ~level:2
	   "Replacing %a ?" !Ast_printer.d_exp expr;
         let type_of_expr = typeOf expr in
         try
           begin
             match unrollType type_of_expr with
             | (TInt _
               | TFloat _
               | TPtr _
               | TEnum _) -> ()
             | _ -> raise Cannot_expand
           end;
           let mkCast ~e ~newt =
             (* introduce a new cast or do not expand [e] *)
             let exp = mkCast e newt in
             if cast_intro then
	       exp
             else
               match exp.enode with
               | CastE _ ->
		   if exp == e (* older cast, no new cast added *) then
                     exp
                   else
		     (* without [cast_intro], introducing such a cast is not
			allowed: do not expand [e] *)
		     raise Cannot_expand
               | _ ->
		   (* remember the change done by [mkCast] (if any).
		      note that [mkCast] make some modifications, even if it
		      does not introduce a new cast. *)
		   exp
           in
           let evaled = !Value.access_expr ki expr in
           let k,m = Cvalue_type.V.find_lonely_binding evaled in
           let can_replace vi =
             vi.vglob ||
               Extlib.may_map
               (Kernel_function.is_formal_or_local vi) ~dft:false
               self#current_kf
           in
           begin
             match k with
               | Base.Var (vi,_) | Base.Initialized_Var (vi,_)
		     when not vi.vlogic && can_replace vi ->
                   if vi.vglob && not (Cilutil.VarinfoSet.mem vi known_globals)
                   then begin
                     let vi = Visitor.visitFramacVarDecl
                       (self:>Visitor.frama_c_visitor) vi
                     in
                     must_add_decl <- Cilutil.VarinfoSet.add vi must_add_decl;
                   end;
                   (* This is a pointer coming for C code *)
                   PropagationParameters.debug
                   "Trying replacing %a from a pointer value {&%a + %a}"
                   !Ast_printer.d_exp expr
                   Base.pretty k
                   Ival.pretty m;
                 let base =  mkAddrOrStartOf (var vi) in
                 let offset = Ival.project_int m in (* these are bytes *)
                 let shifted =
                   if Abstract_interp.Int.is_zero offset then base
                   else
                     let offset,rem =
                       let sizeof_pointed =
                         try
			   Int_Base.project
			     (if isArrayType vi.vtype then
				Bit_utils.osizeof_pointed vi.vtype
                              else
				Bit_utils.osizeof vi.vtype)
                         with
                         | Int_Base.Error_Top
                         | Int_Base.Error_Bottom -> raise Cannot_expand
                       in (Abstract_interp.Int.pos_div offset sizeof_pointed),
                       (Abstract_interp.Int.pos_rem offset sizeof_pointed)
                     in let shifted =
                       if Abstract_interp.Int.is_zero offset
                       then base
                       else let v1 = Abstract_interp.Int.cast
			 ~signed:true
			 ~size:(Abstract_interp.Int.of_int 64)
			 ~value:offset
		       in increm64 base (Abstract_interp.Int.to_int64 v1)
                     in if Abstract_interp.Int.is_zero rem
                     then shifted
                     else let v1 = Abstract_interp.Int.cast
		       ~signed:true
		       ~size:(Abstract_interp.Int.of_int 64)
		       ~value:rem
		     in increm64 (mkCast ~e:shifted ~newt:Cil.charPtrType)
                          (Abstract_interp.Int.to_int64 v1)
                 in let change_to = (* Give it the right type! *)
                   mkCast ~e:shifted ~newt:type_of_expr
                 in
                 PropagationParameters.debug "Replacing %a with %a"
                   !Ast_printer.d_exp expr
                   !Ast_printer.d_exp change_to;
                 ChangeDoChildrenPost (change_to, fun x -> x)
             | Base.Null ->
                 let e =
                   begin
                     try
                       (* This is an integer *)
                       let v = Ival.project_int m in
                       PropagationParameters.debug
			 "Trying replacing %a with a numeric value: %a"
                         !Ast_printer.d_exp expr
                         Abstract_interp.Int.pretty v;
                       try
                         let v1 = Abstract_interp.Int.cast
			   ~signed:true
			   ~size:(Abstract_interp.Int.of_int 64)
			   ~value:v
			 in
                         (* PropagationParameters.debug "XXXXXXXX v=%a v1=%a"
			    Abstract_interp.Int.pretty v
			    Abstract_interp.Int.pretty v1; *)
                         kinteger64 IULongLong (Abstract_interp.Int.to_int64 v1)
                       with Failure _ -> raise Cannot_expand
                     with Ival.Not_Singleton_Int->
		       (* TODO: floats *)
                       raise Cannot_expand
                   end
                 in let change_to =  (* Give it the right type ! *)
                   mkCast ~e ~newt:(type_of_expr)
                 in
                 PropagationParameters.debug "Replacing %a with %a"
                   !Ast_printer.d_exp expr
                   !Ast_printer.d_exp change_to;
                 ChangeDoChildrenPost(change_to,fun x -> x)
             | Base.Cell_class _ | Base.String _
             | Base.Var _ | Base.Initialized_Var _ -> DoChildren

           end
         with Not_found | Cannot_expand -> DoChildren)

  method vvdec v =
    if v.vglob then known_globals <- Cilutil.VarinfoSet.add v known_globals;
    DoChildren

  method vglob_aux g =
    must_add_decl <- Cilutil.VarinfoSet.empty;
    let add_decl l =
      Cilutil.VarinfoSet.fold
        (fun x l ->
           PropagationParameters.feedback ~level:2
             "Adding declaration of global %a" !Ast_printer.d_var x;
           GVarDecl(Cil.empty_funspec(),x,x.vdecl)::l)
        must_add_decl l
    in ChangeDoChildrenPost([g],add_decl)

  method vlval lv =
    let simplify (host,offs as lv) = match host with
      | Mem e -> mkMem e offs (* canonicalize *)
      | Var _ -> lv
    in ChangeDoChildrenPost(lv, simplify)

end

module Result =
  Computation.Hashtbl
    (struct
       type t = Cilutil.StringSet.t * bool
       let hash = Hashtbl.hash
       let equal (s1,b1) (s2,b2) = b1 = b2 && Cilutil.StringSet.equal s1 s2
     end)
    (Datatype.Project)
    (struct
       let size = 7
       let name = "Constant_Propagation"
       let dependencies = [ Value.self;
			    PropagationParameters.CastIntro.self ]
     end)

let journalized_get =
  let get fnames cast_intro =
    Result.memo
      (fun _ ->
	 !Value.compute ();
	 let fresh_project =
	   File.create_project_from_visitor
	     "propagated"
	     (fun prj -> new propagate prj fnames cast_intro)
	 in
	 let ctx = Parameters.get_selection_context () in
	 Project.copy ~only:ctx fresh_project;
	 fresh_project)
      (fnames, cast_intro)
  in
  Journal.register
    "!Db.Constant_Propagation.get"
    (Type.func2 Kernel_type.string_set
       ~label2:("cast_intro",None) Type.bool Project.ty)
    get

(* add labels *)
let get fnames ~cast_intro = journalized_get fnames cast_intro

(** Constant Propagation *)

let compute () =
  PropagationParameters.feedback "beginning constant propagation";
  let fnames = PropagationParameters.SemanticConstFold.get () in
  let cast_intro = PropagationParameters.CastIntro.get () in
  let propagated = !Db.Constant_Propagation.get fnames cast_intro in
  if PropagationParameters.SemanticConstFolding.get () then
    File.pretty ~prj:propagated ();
  PropagationParameters.feedback  "constant propagation done"

let main () =
  let force_semantic_folding =
    PropagationParameters.SemanticConstFolding.get ()
    || not (Cilutil.StringSet.is_empty
	      (PropagationParameters.SemanticConstFold.get ()))
  in
  (* must called the function stored in [Db] for journalisation purpose *)
  if force_semantic_folding then !Db.Constant_Propagation.compute ()

let () =
  Db.Main.extend main;
  Db.register Db.Journalization_not_required Db.Constant_Propagation.get get;
  let _self =
    Db.register_compute
      "Constant_Propagation.compute"
      [ PropagationParameters.SemanticConstFold.self;
      PropagationParameters.SemanticConstFolding.self;
      Result.self ]
      Db.Constant_Propagation.compute
      compute;
  in ()

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
