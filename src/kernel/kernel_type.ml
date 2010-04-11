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

(** An extension of Type library for Frama-C usage *)

open Cil_types

(* ****************************************************************************)
(** {3 Dummy values for registering type values}

    The following values may be used for registering new type value.
    They **must not** be used for any other use. *)
(* ****************************************************************************)

let lexing_pos_dummy = Lexing.dummy_pos

let varinfo_dummy =
  { vorig_name = "dummy";
    vname = "dummy";
    vtype = TVoid [];
    vattr = [];
    vstorage = NoStorage;
    vgenerated = true;
    vglob = false;
    vdefined = false;
    vformal = false;
    vinline = false;
    vdecl = lexing_pos_dummy,lexing_pos_dummy;
    vid = 0;
    vaddrof = false;
    vreferenced = false;
    vdescr = None;
    vdescrpure = false;
    vghost = false;
    vlogic = false;
    vlogic_var_assoc = None }

(* ****************************************************************************)
(** {2 Frama-C types} *)
(* ****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* big_int *)
(* ------------------------------------------------------------------------- *)

(* TODO: move below functor in another file? *)

let pp_big_int _ fmt n = Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
let big_int =
  Type.register
    ~name:"Big_int.big_int" ~value_name:(Some "Kernel_type.big_int")
    ~pp:pp_big_int
    [ Big_int.zero_big_int ]

(* ------------------------------------------------------------------------- *)
(* stmt *)
(* ------------------------------------------------------------------------- *)

let pp_stmt p_caller fmt s =
  let pp fmt =
    Format.fprintf fmt
      "@[<hv 2>fst@;@[<hv 2>(Kernel_function.find_from_sid@;%d)@]@]"
      s.sid
  in
  Type.par p_caller Type.Call fmt pp

let stmt =
  Type.register ~name:"Cil_types.stmt" ~value_name:(Some "Kernel_type.stmt")
    ~varname:(fun _ -> "stmt") ~pp:pp_stmt
    [ Cil.dummyStmt ]

(* ------------------------------------------------------------------------- *)
(* kinstr *)
(* ------------------------------------------------------------------------- *)

let pp_ki p_caller fmt = function
  | Kglobal -> Format.fprintf fmt "Kglobal"
  | Kstmt s ->
      Type.par p_caller Type.Call fmt
	(fun fmt -> Format.fprintf fmt "@[<hv 2>Kstmt@;%a@]"
	   (pp_stmt Type.Call) s)

let kinstr =
  Type.register
    ~name:"Cil_types.kinstr"
    ~value_name:(Some "Kernel_type.kinstr")
    ~varname:(fun _ -> "ki")
    [ Kglobal ]

(* ------------------------------------------------------------------------- *)
(* set *)
(* ------------------------------------------------------------------------- *)

(* TODO: move below functor in another file *)

module MakeSetType(S:Set.S)(E:sig val ty:S.elt Type.t
				  val name: string
				  val value_name : string end)=
struct
  let pp_set p_caller fmt s =
    if S.is_empty s then
      Format.fprintf fmt "%s.empty" E.name
    else
      let pp fmt =
	if S.cardinal s = 1 then
	  Format.fprintf fmt "@[<hv 2>%s.singleton@;%a@]"
	    E.name
	    (Type.pp E.ty Type.Call) (List.hd (S.elements s))
	else
	  Format.fprintf fmt
	   "@[<hv 2>List.fold_left@;(fun acc s -> %s.add s acc)@;%s.empty@;%a@]"
	    E.name
	    E.name
	    (Type.pp (Type.list E.ty) Type.Call)
	    (S.elements s)
      in
      Type.par p_caller Type.Call fmt pp

let ty =
  Type.register ~name:(E.name ^".t")
    ~value_name:(Some E.value_name)
    ~pp:pp_set
    ~varname:(fun _ -> "a_set")
    [ S.empty ]
end

(* ------------------------------------------------------------------------- *)
(* string set *)
(* ------------------------------------------------------------------------- *)

let string_set =
  let module M=MakeSetType(Cilutil.StringSet)
    (struct let name="Cilutil.StringSet"
	    let value_name = "Kernel_type.string_set"
	    let ty = Type.string
     end)
  in
  M.ty

(* ------------------------------------------------------------------------- *)
(* stmt set *)
(* ------------------------------------------------------------------------- *)

let stmt_set =
  let module M=MakeSetType(Cilutil.StmtSet)
    (struct let name="Cilutil.StmtSet"
	    let value_name = "Kernel_type.stmt_set"
	    let ty = stmt
     end)
  in
  M.ty

(* ------------------------------------------------------------------------- *)
(* kernel function *)
(* ------------------------------------------------------------------------- *)

open Db_types

let get_name_kf kf = (Ast_info.Function.get_vi kf.fundec).vname
let pp_kf p_caller fmt kf =
  Type.par p_caller Type.Call fmt
    (fun fmt ->
       Format.fprintf fmt "@[<hv 2>Globals.Functions.find_by_name@;%S@]"
	 (get_name_kf kf))

let kernel_function =
  Type.register ~name:"Kernel_function.t"
    ~value_name:(Some "Kernel_type.kernel_function")
    ~pp:pp_kf
    ~varname:(fun kf -> "kf_" ^ (get_name_kf kf))
    [ { fundec = Definition (Cil.emptyFunction "@dummy@", Cilutil.locUnknown);
	return_stmt = None;
	spec = Cil.empty_funspec ();
	stmts_graph = None } ]

(* ------------------------------------------------------------------------- *)
(* localisation *)
(* ------------------------------------------------------------------------- *)

let pp_loc p_caller fmt loc =
  let pp s kf =
    Type.par p_caller Type.Call fmt
      (fun fmt -> Format.fprintf fmt "@[<hv 2>%s@;%a@]" s (pp_kf Type.Call) kf)
  in
  match loc with
  | VGlobal -> Format.fprintf fmt "Db_types.VGlobal"
  | VLocal kf -> pp "Db_types.VLocal" kf
  | VFormal kf -> pp "Db_types.VFormal" kf

let localisation =
  Type.register ~name:"Db_types.localisation"
    ~value_name:(Some "Kernel_type.localisation")
    ~pp:pp_loc
    [ VGlobal ]

(* ****************************************************************************)
(** {3 Types without pretty-printer} *)
(* ****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* varinfo *)
(* ------------------------------------------------------------------------- *)

let varinfo =
  Type.register
    ~name:"Cil_types.varinfo"
    ~value_name:(Some "Kernel_type.varinfo")
    ~varname:(fun v -> "vi_" ^ v.vorig_name)
    [ varinfo_dummy ]

(* ------------------------------------------------------------------------- *)
(* lval *)
(* ------------------------------------------------------------------------- *)

let lval =
  Type.register ~name:"Cil_types.lval" ~value_name:(Some "Kernel_type.lval")
    ~varname:(fun _ -> "lv")
    [ Var varinfo_dummy, NoOffset ]

(* ------------------------------------------------------------------------- *)
(* Cil_types.file *)
(* ------------------------------------------------------------------------- *)

let cil_file =
  Type.register
    ~name:"Cil_types.file"
    ~value_name:(Some "Kernel_type.cil_file")
    ~varname:(fun _ -> "ast")
    [ { fileName = ""; globals = []; globinit = None; globinitcalled = false } ]

(* ------------------------------------------------------------------------- *)
(* Cabs.file *)
(* ------------------------------------------------------------------------- *)

let cabs_file =
  Type.register
    ~name:"Cabs.file"
    ~value_name:(Some "Kernel_type.cabs_file")
    ~varname:(fun (s, _) -> "cabs_" ^ s)
    [ "", []; "", [ true, Cabs.GLOBANNOT [] ] ]

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
