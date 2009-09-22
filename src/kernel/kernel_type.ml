(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: kernel_type.ml,v 1.21 2009-02-23 12:52:19 uid562 Exp $ *)

(** An extension of Type library for Frama-C usage *)

(* ****************************************************************************)
(** {2 Frama-C types} *)
(* ****************************************************************************)

open Cil_types

let lexing_pos_dummy = Lexing.dummy_pos

(* used to register varinfo type in the journal mechanism. Do not attempt
   to use it in real AST. *)
let varinfo_dummy =
  { vorig_name = "dummy";
    vname = "dummy";
    vtype = TVoid [];
    vattr = [];
    vstorage = NoStorage;
    vglob = false;
    vdefined = false;
    vformal = false;
    vinline = false;
    vfunction_scope = false;
    vdecl = lexing_pos_dummy,lexing_pos_dummy;
    vid = 0;
    vaddrof = false;
    vreferenced = false;
    vdescr = None;
    vdescrpure = false;
    vghost = false;
    vlogic = false;
    vlogic_var_assoc = None }

let pp_big_int _ fmt n = Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
let big_int =
  Type.register
    ~name:"Big_int.big_int" ~value_name:(Some "Kernel_type.big_int")
    ~pp:pp_big_int
    Big_int.zero_big_int

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
    Cil.dummyStmt

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
    Kglobal

let lval =
  Type.register ~name:"Cil_types.lval" ~value_name:(Some "Kernel_type.lval")
    ~varname:(fun _ -> "lv")
    (Var varinfo_dummy, NoOffset)

let pp_string_set p_caller fmt s =
  if Cilutil.StringSet.is_empty s then
    Format.fprintf fmt "Cilutil.StringSet.empty"
  else
    let pp fmt =
      if Cilutil.StringSet.cardinal s = 1 then
	Format.fprintf fmt "@[<hv 2>Cilutil.StringSet.singleton@;%S@]"
	  (List.hd (Cilutil.StringSet.elements s))
      else
	Format.fprintf fmt
	  "@[<hv 2>List.fold_left@;(fun acc s -> Cilutil.StringSet.add s acc)@;Cilutil.StringSet.empty@;%a@]"
	  (Type.pp (Type.list Type.string) Type.Call)
	  (Cilutil.StringSet.elements s)
    in
    Type.par p_caller Type.Call fmt pp

let string_set =
  Type.register ~name:"Cilutil.StringSet.t"
    ~value_name:(Some "Kernel_type.string_set")
    ~pp:pp_string_set
    ~varname:(fun _ -> "string_set")
    Cilutil.StringSet.empty

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
    { fundec = Definition (Cil.emptyFunction "@dummy@", Cilutil.locUnknown);
      return_stmt = None;
      spec = Cil.empty_funspec ();
      stmts_graph = None }

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
    VGlobal

let varinfo =
  Type.register
    ~name:"Cil_types.varinfo"
    ~value_name:(Some "Kernel_type.varinfo")
    ~varname:(fun v -> "vi_" ^ v.vorig_name)
    varinfo_dummy

let cil_file =
  Type.register
    ~name:"Cil_types.file"
    ~value_name:(Some "Kernel_type.cil_file")
    { fileName = ""; globals = []; globinit = None; globinitcalled = false }

let cabs_file =
  Type.register
    ~name:"Cabs.file"
    ~value_name:(Some "Kernel_type.cabs_file")
    ("",[])

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
