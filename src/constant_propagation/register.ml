(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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
      Format.printf "[constant propagation] for function %s@."
        (fundec.svar.vname);
        DoChildren

  method vexpr expr =
    self#on_current_stmt
      DoChildren
      (fun ki ->
         if Cmdline.Debug.get () > 1 then
           Format.printf "Replacing %a ?@."
             !Ast_printer.d_exp expr;
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
             (* introduces a cast if allowed by [cast_intro] *)
             let exp = mkCast e newt in
               if cast_intro then exp
               else
                 match exp with
                   | CastE _ -> if exp == e
                     then (* it isn't a new cast, but an old one *)
                       exp
                     else raise Cannot_expand
                   | _ -> exp
           in
           let evaled = !Value.access_expr ki expr in
           let k,m = Cvalue_type.V.find_lonely_binding evaled in
             begin
               match k with
               | Base.Var (vi,_) | Base.Initialized_Var (vi,_) when not vi.vlogic ->
                   (* This is a pointer coming for C code *)
                   if Cmdline.Debug.get () > 0 then
                     Format.printf "Trying replacing %a from a pointer value {&%a + %a}@."
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
                   in if Cmdline.Debug.get () > 0 then
                       Format.printf "Replacing %a with %a@."
                         !Ast_printer.d_exp expr
                         !Ast_printer.d_exp change_to;
                     ChangeDoChildrenPost (change_to, fun x -> x)
               | Base.Null ->
                   let e =
                     begin
                       try
                         (* This is an integer *)
                         let v = Ival.project_int m in
                         if Cmdline.Debug.get () > 0 then
                           Format.printf "Trying replacing %a with a numeric value: %a@."
                             !Ast_printer.d_exp expr
                             Abstract_interp.Int.pretty v;
                         try
                           let v1 = Abstract_interp.Int.cast
			     ~signed:true
			     ~size:(Abstract_interp.Int.of_int 64)
			     ~value:v
			   in
(*			   Format.printf "XXXXXXXX v=%a v1=%a@."
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
                   in if Cmdline.Debug.get () > 0 then
                       Format.printf "Replacing %a with %a @."
                         !Ast_printer.d_exp expr
                         !Ast_printer.d_exp change_to;
                     ChangeDoChildrenPost(change_to,fun x -> x)
               | Base.Cell_class _ | Base.String _
               | Base.Var _ | Base.Initialized_Var _ -> DoChildren

             end
         with Not_found | Cannot_expand -> DoChildren)

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
       let dependencies = [ Value.self ]
     end)

let journalized_get =
  let get fnames cast_intro =
    Result.memo
      (fun _ ->
	 !Value.compute ();
	 let fresh_project = Project.create "propagated" in
	 File.init_project_from_visitor
	   fresh_project
	   (fun prj -> new propagate prj fnames cast_intro);
	 let ctx = Cmdline.get_selection_context () in
	 Project.copy ~only:ctx fresh_project;
	 fresh_project)
      (fnames, cast_intro)
  in
  Journal.register
    "!Db.Constant_Propagation.get"
    (Type.func Kernel_type.string_set (Type.func Type.bool Project.repr))
    get

(* add labels *)
let get fnames ~cast_intro = journalized_get fnames cast_intro

let main fmt =
  let force_semantic_folding =
    Cmdline.Constant_Propagation.SemanticConstFolding.get ()
    || not (Cilutil.StringSet.is_empty
	      (Cmdline.Constant_Propagation.SemanticConstFold.get ()))
  in
  if force_semantic_folding then begin
    Format.fprintf fmt "@\n[constant propagation] in progress...@.";
    let fnames = Cmdline.Constant_Propagation.SemanticConstFold.get () in
    let cast_intro = Cmdline.Constant_Propagation.CastIntro.get () in
    let propagated = !Db.Constant_Propagation.get fnames cast_intro in
    if Cmdline.Constant_Propagation.SemanticConstFolding.get () then
      File.pretty (Cmdline.CodeOutput.get_fmt ()) ~prj:propagated;
    Format.fprintf fmt "@\n====== CONSTANT PROPAGATED ======@.";
  end

let () = Db.Main.extend main

let options =
  [ "-semantic-const-folding",
    Arg.Unit Cmdline.Constant_Propagation.SemanticConstFolding.on,
    ": force semantic constant propagation and pretty print the new source code.";

    "-semantic-const-fold",
    Arg.String Cmdline.Constant_Propagation.SemanticConstFold.add_set,
    "f1,...,fn : propagate constants only into functions f1,...,fn.";

    "-cast-from-constant",
    Arg.Unit Cmdline.Constant_Propagation.CastIntro.on,
    ": allow introduction of new casts from a folded constant.";
  ]

let () =
  Db.register ~journalize:None Db.Constant_Propagation.get get;
  Options.add_plugin
    ~name:"semantic constant folding"
    ~descr:"propagates semantically constants"
    options

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
