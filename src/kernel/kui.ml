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

(* $Id: kui.ml,v 1.103 2008-04-01 09:25:21 uid568 Exp $ *)
open Cil_types
type appl = Project.t
type file = File.t
type files = file list

type proj = Db.Slicing.Project.t
type projs = Db.Slicing.Project.t list

type varinfo = Cil_types.varinfo
type varinfos = varinfo list

type glob = varinfo * Cil_types.initinfo
type globs = glob list option
type func = Db_types.kernel_function
type funcs = func list option

type value = Cvalue_type.V.t
type stmt = Cil_types.stmt * func

type location = Locations.location
type zones = Locations.Zone.t
type zone = zones (* with only one key *)

type from = zone * bool * zones
type froms = func * Function_Froms.t

type pdg =  Db.Pdg.t

(** C Symbol *)
module Varinfo = struct
  type t = varinfo

  exception Invalid of t

  (** Acces functions *)

  let get_name x = x.Cil_types.vname
  let get_id x = x.Cil_types.vid
  let get_file_def x = (fst x.Cil_types.vdecl).Lexing.pos_fname

  (** Test functions *)

  let is_glob_var x =
    if x.Cil_types.vlogic
    then false
    else if x.Cil_types.vglob then
      match x.Cil_types.vtype with
        | Cil_types.TFun _ -> false
        | _ -> true
    else
      false

  let is_func = Ast_info.is_function_type

  let is_void_func x =
    match x.Cil_types.vtype with
      | Cil_types.TFun (Cil_types.TVoid (_),_,_,_) -> true
      | _ -> false

  (* varinfo vid is unique for an application *)
  let compare x1 x2 = Pervasives.compare (get_id x1) (get_id x2)
end

(** C Varinfos *)
module Varinfos = struct
  type t = varinfos
  type elt = varinfo

  (** Iteration functions *)

  let iter = List.iter
  let fold = List.fold_right
  let filter = List.filter

  exception Found of elt
  let find_elt f x =
    try
      iter (fun y -> if f y then (raise (Found (y)))) x;
      raise Not_found
    with Found founded -> founded

  let find_elt_from_name name = find_elt (fun y -> String.compare name (Varinfo.get_name y) = 0)

end

(** C global variables *)
module Glob = struct
  type t = glob

  exception Uninitialized of t

  (** Acces functions *)

  let get_symb ((vi, _ii):t) = vi

  (** Test functions *)

  let has_init (( _vi, ii):t) =
    begin
      match ii.Cil_types.init with
        | None -> false
        | _ -> true
    end

  (** Shortcuts *)

  let get_name x = Varinfo.get_name (get_symb x)
  let get_id x = Varinfo.get_id (get_symb x)
  let get_file_def x =
    if not (has_init x) then raise (Uninitialized x) ;
    Varinfo.get_file_def (get_symb x)
  let compare x1 x2 = Varinfo.compare (get_symb x1) (get_symb x2)
end

module Globs  = struct
  (* C global variables *)
  type t = globs
  type elt = glob

  (** Iteration functions *)

  let iter f x =
    begin
      match x with
        | Some x -> List.iter f x
        | _ ->
            let fg vi ii = f (vi, ii)
            in Globals.Vars.iter fg
    end

  let fold f x acc =
    let ac = ref acc in
    let fg b = ac := f b (!ac) ; ()
    in iter fg x ; !ac

  let filter f x = Some (fold (fun y a -> if f y then y::a else a) x [])

  exception Found of elt
  let find_elt f x =
    try
      iter (fun y -> if f y then (raise (Found (y)))) x;
      raise Not_found
    with Found founded -> founded

  let find_elt_from_name name = find_elt (fun y -> String.compare name (Glob.get_name y) = 0)

end

(** Memory zone identifier *)
module Base = Base

module Location = struct
  type t = location

  (** {b Constructors:} *)

  let from_symb =
    Locations.loc_of_varinfo
    (** Make location related to the symbol [s1]. *)

  let from_base =
    Locations.loc_of_base
    (** Make location related to the memory zone identifier [b1]. *)

end

(** Memory zone relative to a memory zone identifier *)
module Zone = struct
  type t = zone

  (** Make functions *)

  (** Test functions *)

  let is_equal = Locations.Zone.equal
  let is_included = Locations.Zone.is_included
  let intersects = Locations.Zone.intersects

  (** Access function *)

  let get_base z =
    try
      let f zid a =
        match a with
          | None -> Some zid
          | _  -> assert (false)
      in match Locations.Zone.fold_i (fun k _ acc -> f k acc) z None with
        | Some zid -> zid
        | _ -> assert (false)
    with _ -> assert (false)

  (** Others *)

  let pretty = Locations.Zone.pretty

end

(** Memory zones *)
module Zones = struct
  type elt = zone
  type t = zones

  exception Base_Error of t

  (** Values: *)

  let empty = Locations.Zone.bottom

  (** Constructors *)

  let from_zone x = x

  let from_locations loc =
    Locations.valid_enumerate_bits loc

  (** Test functions *)

  let is_equal = Locations.Zone.equal
  let is_included = Locations.Zone.is_included
  let intersects = Locations.Zone.intersects

  let make_locations_from_symb symb =
    Locations.loc_of_varinfo symb

  (** Iteration functions *)

  let fold f x =
    try Locations.Zone.fold_enum_by_base f x with
      | Locations.Zone.Error_Top -> raise (Base_Error x)

  let fold_base f x =
    try Locations.Zone.fold_i (fun k _ acc -> f k acc) x with
      | Locations.Zone.Error_Top -> raise (Base_Error x)

  (** Others *)

  let union = Locations.Zone.join

  let join = Locations.Zone.join

  let pretty = Locations.Zone.pretty

end

(** Dependencies related to an output of a function. *)
module From = struct
  type t = from

  (** Acces functions *)

  let get_output_zone (zone, _b, _zones) = zone

  let get_input_zones (_zone, _b, zones) = zones

  let may_be_unmodified (_zone, b, _zones) = b

  let pretty fmt (zone, b, zones) =
    Format.fprintf fmt "@[%a FROM @[%a@](and default:%b)@]"
      Zone.pretty zone Zone.pretty zones  b

end

(** C functions *)
module Func = struct
  type t = func

  exception Never_Called of t
  exception Undefined of t
  exception Void_Func of t

  (** {b Constructors:} *)

  let find_from_func_symb x =
    if not (Varinfo.is_func x)
    then raise (Varinfo.Invalid x) ;
    Globals.Functions.get x

  let find_from_local_var_or_param_symb x =
    try Globals.FileIndex.kernel_function_of_local_var_or_param_varinfo x with
      | _ -> raise (Varinfo.Invalid x)

  (** Test functions *)

  let has_def = Kernel_function.is_definition

  (** Acces functions *)

  let get_symb = Kernel_function.get_vi

  (** Shortcuts *)

  let compare x1 x2 = Varinfo.compare (get_symb x1) (get_symb x2)
  let get_name x = Varinfo.get_name (get_symb x)
  let is_void_func x = Varinfo.is_void_func (get_symb x)

  let get_file_def x =
    if not (has_def x) then raise (Undefined x) ;
    Varinfo.get_file_def (get_symb x)

  (** Test functions *)

  let never_called x = not (!Db.Value.is_called x)

  let precond_not_never_called x =
    if never_called x then raise (Never_Called x)

  let never_terminates x =
    precond_not_never_called x;
    !Db.Value.never_terminates x

  (** Acces functions *)

  let get_return_locations kf =
    try
      Db.Value.find_return_loc kf
    with Db.Value.Void_Function -> raise (Void_Func kf)

  let get_params x =
    match x.Db_types.fundec with
      | Db_types.Definition (fundec, _) -> fundec.Cil_types.sformals
      | _ -> raise (Undefined x)

  let get_locals x =
    match x.Db_types.fundec with
      | Db_types.Definition (fundec, _) -> fundec.Cil_types.slocals
      | _ -> raise (Undefined x)

  let get_internal_inputs x =
    precond_not_never_called x;
    assert false
(* Db.InOutContext.get_over_input_context (!Db.InOutContext.get_internal x) *)

  let get_external_inputs x =
    precond_not_never_called x;
    assert false
 (*   Db.InOutContext.get_over_input_context (!Db.InOutContext.get_external x)*)

  let get_functional_inputs x =
    precond_not_never_called x;
    !Db.Inputs.get_external x

  let get_internal_outputs x =
    precond_not_never_called x;
    !Db.Outputs.get_internal x

  let get_external_outputs x =
    (* Since the returned zone isn't into the result of [!Db.Out.get_external],
       hack to simulate its presence. *)
    precond_not_never_called x;
    let actual_result = !Db.Outputs.get_external x in
      if not (is_void_func x)
      then
        let ret_zones = Zones.from_locations (get_return_locations x) in
          if Zones.intersects ret_zones (get_internal_outputs x)
          then
            (* VP: local variables should never be displayed as
               external outputs. What is the purpose of this code?
             *)
            Zones.join ret_zones actual_result
          else
            actual_result
      else
        actual_result

  let get_sure_external_outputs x =
    precond_not_never_called x;
    assert false
      (*Db.InOutContext.get_under_output_context_if_termination (!Db.InOutContext.get_external x)*)


  let get_functional_outputs x =
    precond_not_never_called x;
    assert (false) (* TO DO *)

  let get_froms x =
    precond_not_never_called x;
    x, !Db.From.get x

end

(** Value domain *)
module Value = struct
  type t = value

  (** Test/comparaison functions *)
  let is_equal x1 x2 = Cvalue_type.V.equal x1 x2
  let is_included = Cvalue_type.V.is_included
  let intersects = Cvalue_type.V.intersects

  (** Others *)

  let pretty = Cvalue_type.V.pretty

end

(** Statement *)
module Stmt = struct
  type t = stmt

  exception Never_Executed

  (** Constructors *)

  (** Acces functions *)

  let get_func (_ki, kf) = kf

  let find_last_from_func kf =
    let ki = Kernel_function.find_return kf
    in if (!Db.Value.is_called kf)
        && (Db.Value.is_accessible (Cil_types.Kstmt ki))
      then (ki, kf)
      else raise Never_Executed

  let find_first_from_func kf =
    let ki = Kernel_function.find_first_stmt kf
    in if (!Db.Value.is_called kf)
        && (Db.Value.is_accessible (Cil_types.Kstmt ki))
      then (ki, kf)
      else raise Never_Executed

  let get_locations str (ki, kf) =
    let lval_term = !Db.Properties.Interp.lval kf ki str in
    let lval = !Db.Properties.Interp.term_lval_to_lval lval_term in
    !Db.Value.lval_to_loc ~with_alarms:CilE.warn_none_mode (Cil_types.Kstmt ki) lval

  let get_value_before_stmt_of_locations (ki, _kf) loc =
    !Db.Value.access_location (Cil_types.Kstmt ki) loc

  let get_value_after_stmt_of_locations (ki, _kf) loc =
    !Db.Value.access_location_after (Cil_types.Kstmt ki) loc

  let get_called_funcs (ki, _kf) =
    Some (match ki with
          | {Cil_types.skind=Cil_types.Instr (Cil_types.Call (_,expr_f,_,_))} ->
              snd (!Db.Value.expr_to_kernel_function
                     (Cil_types.Kstmt ki)
                     ~with_alarms:CilE.warn_none_mode
                     ~deps:None
                     expr_f)
          | _ -> [])
end

(** Dependencies related to a function. *)
module Froms = struct
  type elt = from
  type t = froms

  exception Base_Error of t

  (** Acces functions *)

  let get_input_zones (_fct, d) z =
    !Db.From.access z d.Function_Froms.deps_table

  (** Iteration functions *)

  let fold f ((_fct, d) as x) acc =
    try
      let g z (b, zones) a = f (z, b, zones) a
      in Lmap_bitwise.From_Model.fold g d.Function_Froms.deps_table acc ;
    with
        Lmap_bitwise.From_Model.Cannot_fold -> raise (Base_Error x)

  (** Others *)

  let pretty fmt (fct, _d) =
    !Db.From.pretty fmt fct

end

module Funcs = struct
  type t = funcs
  type elt = func

  (** Acces functions *)

  let find_callers x =
    if Func.never_called x then raise (Func.Never_Called x) ;
    Some (List.map fst (!Db.Value.callers x))

  (** Iteration functions *)

  let iter f x =
    begin
      match x with
        | Some x -> List.iter f x
        | _ -> Globals.Functions.iter f
    end

  let fold f x acc =
    let ac = ref acc in
    let fg b = ac := f b (!ac) ; ()
    in iter fg x ; !ac

  let filter f x = Some (fold (fun x a -> if f x then x::a else a) x [])

  exception Found of elt
  let find_elt f x =
    try
      iter (fun y -> if f y then (raise (Found y))) x;
      raise Not_found
    with Found founded -> founded

  let find_elt_from_name name =
    find_elt (fun y -> String.compare name (Func.get_name y) = 0)
end

(** C modules *)
module File = struct
  type t = file

  (** Acces functions *)

  let get_name = File.name
  let get_all = File.get_all

  let get_globs x : globs =
    Some (Globals.FileIndex.get_globals ~filename:(get_name x))
    (* C global variables defined, declared or used by the C module *)

  let get_funcs x : funcs =
    Some (Globals.FileIndex.get_functions ~filename:(get_name x))
    (* C global variables defined, declared or used by the C module *)

  let pretty = File.pretty

  (** Test functions *)

  (* file name is unique for an application *)
  let compare x1 x2 = String.compare (get_name x1) (get_name x2)
end

module Files = struct
  type t = files
  type elt = file

  (** Iteration functions *)

  let iter = List.iter
  let fold = List.fold_right
  let filter = List.filter

  exception Found of elt
  let find_elt f x =
    try
      iter (fun y -> if f y then (raise (Found (y)))) x;
      raise Not_found
    with Found founded -> founded

  let find_elt_from_name name =
    find_elt (fun y -> String.compare name (File.get_name y) = 0)

end

(** C applications *)
module Appl = struct
  type t = appl

  (** Management functions *)

  let get_current () = Project.current()
    (** Current application *)

  let set_current appl = Project.set_current appl

  (** Acces functions *)

  let get_files () = File.get_all ()
  let get_globs () : globs = None
  let get_funcs () : funcs = None

  let find_file_from_name name = Files.find_elt_from_name name (get_files ())
  let find_glob_from_name name = Globs.find_elt_from_name name (get_globs ())
  let find_func_from_name name = Funcs.find_elt_from_name name (get_funcs ())

  let find_stmt_from_num sid =
    let ki,kf as s = Kernel_function.find_from_sid sid in
    if !Db.Value.is_called kf && Db.Value.is_accessible (Cil_types.Kstmt ki) then s
    else raise Stmt.Never_Executed

end

module Project = struct
  type t = proj
  type t_set = projs

  let make name = Db.Slicing.Project.mk_project name

  let is_func_called = !Db.Slicing.Project.is_called
  let has_persistent_selection = !Db.Slicing.Project.has_persistent_selection
  let change_slicing_level p f l =
    try !Db.Slicing.Project.change_slicing_level p f l
    with SlicingTypes.ExternalFunction _
      | SlicingTypes.WrongSlicingLevel -> ()

  (**    Build a new application from the project results, and print the result
         in a file if [filename_opt] is specified, on [stdout] otherwise.
  *)
  let export project filename =
    let new_appli = !Db.Slicing.Project.extract "slicing_proj" project in
    let cout = open_out filename in
    SlicingParameters.result "Slicing result printed in %s" filename ;
    let out = Format.formatter_of_out_channel cout in
    File.pretty out ~prj:new_appli;
    Format.pp_print_flush out ();
    close_out cout

  let pretty = !Db.Slicing.Project.pretty
end

module Mark = struct
  type t = SlicingTypes.sl_mark

  let make = !Db.Slicing.Mark.make

  let is_bottom = !Db.Slicing.Mark.is_bottom
  let is_spare = !Db.Slicing.Mark.is_spare
  let is_ctrl = !Db.Slicing.Mark.is_ctrl
  let is_data = !Db.Slicing.Mark.is_data
  let is_addr = !Db.Slicing.Mark.is_addr

  let pretty = !Db.Slicing.Mark.pretty
end

module Select = struct
  type t = Db.Slicing.Select.t_set

  let empty = !Db.Slicing.Select.empty_selects

  let from_stmt s ~spare (ki, kf) = !Db.Slicing.Select.select_stmt s ~spare ki kf
  let from_stmt_ctrl s ~spare (ki, kf) = !Db.Slicing.Select.select_stmt_ctrl
    s ~spare ki kf
  let from_stmt_zone s m z ~before (ki, kf) =
    !Db.Slicing.Select.select_stmt_zone s m z ~before ki kf
  let from_stmt_annots s m ~spare ~ai ~user_assert
      ~slicing_pragma ~loop_inv ~loop_var (ki, kf) =
    !Db.Slicing.Select.select_stmt_annots s m
      ~spare ~ai ~user_assert ~slicing_pragma ~loop_inv ~loop_var ki kf

  let from_func_return = !Db.Slicing.Select.select_func_return
  let from_func_calls_to = !Db.Slicing.Select.select_func_calls_to
  let from_func_calls_into = !Db.Slicing.Select.select_func_calls_into
  let from_func_zone = !Db.Slicing.Select.select_func_zone
  let from_func_annots = !Db.Slicing.Select.select_func_annots

  let pretty format set =
    let pretty_format select = !Db.Slicing.Select.pretty format select
    in !Db.Slicing.Select.iter_selects_internal pretty_format set
end

module Slice = struct
  type t = Db.Slicing.Slice.t
  let pretty = !Db.Slicing.Slice.pretty

  let get_func = !Db.Slicing.Slice.get_function

  let find_mark_from_stmt slice (ki, kf) =
    if get_func slice <> kf then raise Not_found;
    !Db.Slicing.Slice.get_mark_from_stmt slice ki

  let find_mark_from_local_or_param slice vi  =
    if Varinfo.is_glob_var vi then raise Not_found;
    try
      !Db.Slicing.Slice.get_mark_from_formal slice vi
    with Not_found -> !Db.Slicing.Slice.get_mark_from_local_var slice vi

  let find_called_slice slice (ki, _kf) =
    !Db.Slicing.Slice.get_called_slice slice ki

  let find_called_funcs slice (ki, _kf) =
    Some (!Db.Slicing.Slice.get_called_funcs slice ki)

end

module Slices = struct
  type t = Db.Slicing.Slice.t list
  type elt = Slice.t

  let find_from_project_func =
    !Db.Slicing.Slice.get_all
  let find_callers =
    !Db.Slicing.Slice.get_callers

  (** Iteration functions *)

  let iter = List.iter
  let fold = List.fold_right
  let filter = List.filter

end

module Request = struct
  (** Slicing resquests are part of a slicing project.
      So, user requests affect slicing project. *)

  let apply_all = !Db.Slicing.Request.apply_all_internal

  let add_persistent_selection = !Db.Slicing.Request.add_persistent_selection

  let pretty = !Db.Slicing.Request.pretty
end

(* Appl test: open Kui *)
let appl_files () = Appl.get_files ()
let appl_globs () = Appl.get_globs ()
let appl_funcs () = Appl.get_funcs ()

(* File test: open Kui *)
let name_of_appl_files () =
  Files.fold (fun x a -> (File.get_name x)::a) (appl_files ()) []

(* Glob test: open Kui *)
let info_of_globs globs =
  Globs.fold
    (fun x a -> (Glob.get_name x,
                 if Glob.has_init x then Glob.get_file_def x else "")::a)
    globs []

let info_of_appl_globs () =
   info_of_globs (appl_globs ())

let info_of_globs_by_appl_files ()  =
  let f file acc =
    (info_of_globs (File.get_globs file),
     File.get_name file)::acc
  in Files.fold f (appl_files ())[]

(* Func test: open Kui *)
let info_of_appl_funcs () =
  Funcs.fold
    (fun x a -> (Func.get_name x, Func.has_def x, Func.never_called x)::a)
    (appl_funcs ()) []

let info_of_funcs_by_appl_files ()  =
  let f file acc =
    (File.get_name file,
     Funcs.fold (fun x a -> (Func.get_name x)::a) (File.get_funcs file) []
    )::acc
  in Files.fold f (appl_files ())[]

let callers_info_of_called_funcs () =
  let called_funcs =
    Funcs.filter (fun fct -> not (Func.never_called fct)) (appl_funcs ())
  and callers_info fct acc =
    (Func.get_name fct,
     Funcs.fold (fun x a -> (Func.get_name x)::a) (Funcs.find_callers fct) [])::acc
  in Funcs.fold callers_info called_funcs []


(* Func test: open Kui *)
(* For Base extraction... *)
let in_out_info_of_appl_funcs filter get_in_out =
  let called_funcs = Funcs.filter filter (appl_funcs ())
  and in_out_info fct acc =
    (Func.get_name fct,
     let r =
       Format.printf "%s:[" (Func.get_name fct);
       try
         let zone_info z a =
           Format.printf "%a;" Base.pretty z;
           (match z with
              | Base.Var (s,_) | Base.Initialized_Var (s,_) ->
                  s.Cil_types.vname
	      | Base.Cell_class _ -> "cell_class"
              | Base.String _ -> "char[]"
              | Base.Null -> "0x")::a
         in Zones.fold_base zone_info (get_in_out fct) []
       with Zones.Base_Error zones -> Format.printf "%a;" Zones.pretty zones;
         ["0x-char[]-&var"]
     in Format.printf "]\n";
       r
    )::acc
  in Funcs.fold in_out_info called_funcs []

(* Extract the output Base for each functions. *)
let output_info_of_appl_funcs () =
  in_out_info_of_appl_funcs
    (fun fct -> not (Func.never_called fct))
    Func.get_external_outputs

(* Extract the input Base for each functions. *)
let input_info_of_appl_funcs () =
  in_out_info_of_appl_funcs
    (fun fct -> not (Func.never_called fct))
    Func.get_external_inputs

(* Extract the Base of zones1 which intersects zones2 for each functions. *)
let in_out_member_info_of_appl_funcs filter get_zones1 get_zones2 =
  let called_funcs = Funcs.filter filter (appl_funcs ())
  and inout_info fct acc =
    try
      (Func.get_name fct,
       let r =
         Format.printf "%s:[" (Func.get_name fct);
         try
           let zones_out = get_zones2 fct
           in let zone_in z_in a =
               if Zones.intersects zones_out (Zones.from_zone z_in)
               then (* There is an intersection between this [z_in] and the [zones_out] *)
                 let z = Zone.get_base z_in
                 in Format.printf "%a;" Base.pretty z;
                   (match z with
                      | Base.Var (s,_) | Base.Initialized_Var (s,_) ->
                          s.Cil_types.vname
                      | Base.String _ -> "char[]"
		      | Base.Cell_class _ -> "cell_class"
                      | Base.Null -> "0x")::a
               else
                 a
           in Zones.fold zone_in (get_zones1 fct) []
         with Zones.Base_Error zones -> Format.printf "%a;" Zones.pretty zones;
           ["0x-char[]-&var"]
       in Format.printf "]\n";
         r
      )::acc
    with Func.Void_Func _ -> acc
  in Funcs.fold inout_info called_funcs []

(* Extract the Base of inputs which intersects the outputs for each functions. *)
let inout_info_of_appl_funcs () =
  in_out_member_info_of_appl_funcs
    (fun fct -> not (Func.never_called fct))
    Func.get_external_inputs Func.get_external_outputs

(* From test: open Kui *)
(* Extract from dependencies for the returned value of each functions. *)
let from_result_info_of_appl_funcs () =
  in_out_info_of_appl_funcs
    (fun fct -> (not (Func.never_called fct)) &&
       (not (Func.is_void_func fct)))
    (fun fct -> let loc = Func.get_return_locations fct in
     let zone = Zones.from_locations loc in
       Froms.get_input_zones (Func.get_froms fct) zone)
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
