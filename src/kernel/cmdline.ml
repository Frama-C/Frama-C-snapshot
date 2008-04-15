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

(* $Id: cmdline.ml,v 1.222 2008/12/09 15:24:13 uid562 Exp $ *)

(** Bunch of values which may be initialized through command line. *)

(* ****************************************************************************
   Note to the developper: how to add a new option ?
   =================================================

   * If the options is shared by all projects:
   simply implement the signature [S] and use a reference for the option

   * If the option may to not be shared by all projects:
   - if its datatype is simple:
   simply apply one of the predefined functors (e.g. [False]): the option is a
   module implementing the signature [S].
   - if its datatype is not so simple:
   have a look at the predefined functor IndexedVal, which uses strings as
   indexes for the valid options, and stores the current string index in
   the project's state. Even if you need more than that, this should be an
   example on how to use  [Project.State.Register] to have your options
   properly registered in the current project.
   ************************************************************************* *)

open Extlib

let selection = ref Project.Selection.empty
let get_selection () = !selection
let add_option select =
  selection := select Kind.Do_Not_Select_Dependencies !selection

module Selected_Options = Hook.Make(struct end)
let clear_selected_options = Selected_Options.clear
let set_selected_options = Selected_Options.apply
let nb_selected_options = Selected_Options.length

(* ************************************************************************* *)
(** {2 Generic functors} *)
(* ************************************************************************* *)

module type S = sig
  type t
  val set: t -> unit
  val get: unit -> t
  val clear: unit -> unit
  val is_set: unit -> bool
  include Project.Computation.OUTPUT
  val equal: t -> t -> bool
  val unsafe_set: t -> unit
end

type 'a option_accessor = {get : unit -> 'a ; set : 'a -> unit }

type kind =
  | Bool of bool option_accessor
  | Int of int option_accessor
  | String of string option_accessor
  | StringSet of string option_accessor
  | StringList of string option_accessor

type t = kind * string
(*
type tree =
  | Node of tree list * string
  | Leaf of kind * string * string
*)
let options : t list ref = ref []
let iter_on_options f = List.iter f !options

let register_dynamic = ref false

module Build
  (X:sig
     type t
     val default: unit -> t
     val name: string
     val fun_ty: t Type.t
     val equal: t -> t -> bool
   end)
  : S with type t = X.t =
struct

  include Computation.Ref
    (struct
       include Project.Datatype.Persistent(X)
       let default = X.default
       let equal = X.equal
     end)
    (struct
       let name = X.name
       let dependencies = []
     end)

  type t = data

  let () = add_option select

  let is_set () = not (X.equal (X.default ()) (get ()))

  let unsafe_set x =
    if not (X.equal x (get ())) then begin
      set x;
      Selected_Options.extend (fun () -> set x)
    end

  let force_set x =
    let do_set x =
      set x;
      Project.clear
	~only:(Project.Selection.singleton
		 self
		 Kind.Only_Select_Dependencies)
        ()
    in
      do_set x;
      (*TODO: change this hack BTS 352 *)
      if X.name<>"Files" then Selected_Options.extend (fun () -> do_set x)

  let force_set =
    Journal.register
      ~use_apply:!register_dynamic
      ("Cmdline." ^ X.name ^ ".set") (* Journalization is prettier with .set
                                        than with force_set *)
      (Type.func X.fun_ty Type.unit)
      force_set

  let set x =
    if not (X.equal x (get ())) then force_set x

  let unsafe_set =
    Journal.register
      ~use_apply:!register_dynamic
      ("Cmdline." ^ X.name ^ ".unsafe_set")
      (Type.func X.fun_ty Type.unit)
      unsafe_set

  let clear () = set (X.default ())
  let equal = X.equal

end

(** {3 Boolean} *)

module type BOOL = sig
  include S with type t = bool
  val on: unit -> unit
  val off: unit -> unit
end

module Bool
  (X:sig
     val default: bool
     val name: string
   end) =
struct
  include Build
    (struct
       type t = bool
       let default () = X.default
       let name = X.name
       let equal = (=)
       let fun_ty = Type.bool
     end)
  let on () = set true
  let off () = set false
  let () = options := (Bool{get=get;set=set}, name) :: !options
end

(** Build a boolean option initialized to [false].
    @plugin development guide *)
module False(X: sig val name: string end) =
  Bool(struct include X let default = false end)

(** Build a boolean option initialized to [true].
    @plugin development guide *)
module True(X: sig val name: string end) =
  Bool(struct include X let default = true end)

(** {3 Integer} *)

module type INT = sig
  include S with type t = int
  val incr: unit -> unit
end

(** Build an integer option.
    @plugin development guide *)
module Int
  (X: sig
     val default: int
     val name: string
   end) =
struct
  include Build
    (struct
       type t = int
       let default () = X.default
       let name = X.name
       let equal = (=)
       let fun_ty = Type.int
     end)
  let incr () = set (succ (get ()))
  let () = options := (Int{get=get;set=set}, name) :: !options
end

(** Build an integer option initialized to [0].
    @plugin development guide *)
module Zero(X: sig val name: string end) =
  Int(struct include X let default = 0 end)

(** {3 String} *)

module type STRING = S with type t = string

(** Build a string option.
    @plugin development guide *)
module String(X: sig val default: string val name: string end) = struct
  include Build
    (struct
       type t = string
       let name = X.name
       let default () = X.default
       let equal = (=)
       let fun_ty = Type.string
     end)
  let () = options := (String{get=get;set=set}, name) :: !options
end

(** Build a string option initialized to [""].
    @plugin development guide *)
module EmptyString(X: sig val name: string end) =
  String(struct include X let default = "" end)

(** {3 String set and string list} *)

module type GEN_STRING_SET = sig
  include S
  val set_set: string -> unit
  val get_set: ?sep:string -> unit -> string
  val add: string -> unit
  val add_set: string -> unit
  val iter: (string -> unit) -> unit
  val fold: (string -> 'a -> 'a) -> 'a -> 'a
end

module type STRING_SET = sig
  include GEN_STRING_SET with type t = Cilutil.StringSet.t
  val is_empty: unit -> bool
  val remove: string -> unit
  val remove_set: string -> unit
end

module type STRING_LIST = GEN_STRING_SET with type t = string list

(** Build an option as a set of strings, initialized to the empty set. *)
module StringSet(X: sig val name: string end) = struct
  open Cilutil
  include Build
    (struct
       let default () = StringSet.empty
       include StringSet include X let fun_ty = Kernel_type.string_set
     end)

  let add x = set (StringSet.add x (get ()))
  let remove x = set (StringSet.remove x (get()))

  let split_set = Str.split (Str.regexp "[ \t]*,[ \t]*")

  let set_and_split x acc = set (List.fold_right StringSet.add (split_set x) acc)
  let set_set x = set_and_split x StringSet.empty
  let add_set x = set_and_split x (get ())

  let get_set ?(sep=", ") () =
    StringSet.fold
      (fun s acc -> if acc<>"" then s^sep^acc else s) (get ()) ""

  let remove_set x =
    set (List.fold_right StringSet.remove (split_set x) (get()))
  let is_empty () = StringSet.is_empty (get ())
  let iter f = StringSet.iter f (get ())
  let fold f acc = StringSet.fold f (get ()) acc

  let () = options := (StringSet{get=(fun () -> get_set ());set=set_set}, name) :: !options

end

(** Build an option as a list of strings, initialized to the empty list. *)
module StringList(X: sig val name: string end) = struct
  include Build
    (struct
       let default () = []
       include X
       let fun_ty = Type.list Type.string
       type t = string list
       let equal = (=)
     end)

  let add =
    Journal.register
      ("Cmdline." ^ X.name ^ ".add")
      (Type.func Type.string Type.unit)
      (fun x -> set (x :: (get ())))

  let split_set = Str.split (Str.regexp "[ \t]*,[ \t]*")

  let set_and_split x acc =
    set (List.fold_right (fun a b -> a::b) (split_set x) acc)
  let set_set x = set_and_split x []
  let add_set x = set_and_split x (get ())

  let get_set ?(sep=", ") () =
    List.fold_right
      (fun s acc -> if acc<>"" then s^sep^acc else s) (get ()) ""
  let iter f = List.iter f (get ())
  let fold f acc = List.fold_right f (get ()) acc

  let () = options := (StringList{get=(fun () -> get_set ());set=set_set}, name) :: !options

end

(** {3 Complex values indexed by strings} *)

(** option interface *)
module type INDEXED_VAL = sig
  include STRING
  type value (** the real type for the option*)
  val add_choice: string -> value -> unit
    (** adds a new choice for the option. *)
  val get_val: unit -> value
end

(** input signature for [IndexedVal] *)
module type COMPLEX_VALUE = sig
  type t (** the type to be serialized *)
  val default_val: t (** the default value *)
  val default_key: string (** the default index *)
  val name: string (** name of the associated state *)
  val fun_ty: t Type.t
end

(** @plugin development guide *)
module IndexedVal (V:COMPLEX_VALUE):INDEXED_VAL with type value = V.t = struct

  type value = V.t

  let options = Hashtbl.create 13
  let add_choice k v  = Hashtbl.add options k v
  let () = add_choice V.default_key V.default_val

  let create () = ref V.default_key

  let curr_choice = ref (create())

  module StateAux = struct
    let name = V.name
    let create = create

    type t = string ref

    let get () = !curr_choice
    let set s =
      if s != get () then
	if Hashtbl.mem options !s then
          curr_choice:=s
	else
          Printf.eprintf
            "Warning: %s: identifier %s is not a valid index for this option. \
         Option is unchanged.\n" V.name !s

    let copy s = ref !s
    let rehash s = copy s
    let clear tbl = tbl:= V.default_key
    let dependencies = []
  end

  let equal = (=)

  module State =
    Project.Computation.Register
      (Datatype.Ref(Datatype.String))(StateAux)(StateAux)

  type t = string

  let () = add_option State.select

  include State

  let get () = !(!curr_choice)
  let get_val () = Hashtbl.find options (get())

  let set s =
    if s <> get () then
      if Hashtbl.mem options s then
	!curr_choice:=s
      else
	Printf.eprintf
          "Warning: %s: identifier %s is not a valid index for this option. \
         Option is unchanged.\n" V.name s

  let clear () = !curr_choice:=V.default_key
  let is_set () = !(!curr_choice) <> V.default_key

  let unsafe_set = set

end

(** {3 Interface for dynamic plugins} *)

module Dynamic = struct

  module L = List
  open Type
  module List = L (* [List] of stdlib hides the one of [Type]. *)

  let tbl = FunTbl.create 97

  module Register = struct

    let register_common sign name get set clear is_set=
      FunTbl.register tbl (name ^ ".get") (func unit sign) get;
      FunTbl.register tbl (name ^ ".set") (func sign unit) set;
      FunTbl.register tbl (name ^ ".clear") (func unit unit) clear;
      FunTbl.register tbl (name ^ ".is_set")(func unit bool) is_set

    let register_int name on off =
      FunTbl.register tbl (name ^ ".on") (func unit unit) on;
      FunTbl.register tbl (name ^ ".off") (func unit unit) off

    module False(X: sig val name: string end)= struct
      let () = register_dynamic := true
      include Bool(struct include X let default = false end)
      let () = register_dynamic := false
      let () =
	register_common bool X.name get set clear is_set;
	(* register BOOL fonctions *)
	register_int X.name on off
    end

    module True(X: sig val name: string end)= struct
      let () = register_dynamic := true
      include Bool(struct include X let default = true end)
      let () = register_dynamic := false
      let () =
	register_common bool X.name get set clear is_set;
	(* register BOOL fonctions *)
	register_int X.name on off
    end

    module Zero(X: sig val name: string end) = struct
      let () = register_dynamic := true
      include Int(struct include X let default = 0 end)
      let () = register_dynamic := false
      let () =
	register_common int X.name get set clear is_set;
	(* register INT fonctions *)
	FunTbl.register tbl (X.name ^ ".incr") (func unit unit)  incr
    end

    module EmptyString(X: sig val name: string end) = struct
      let () = register_dynamic := true
      include String(struct include X let default = "" end)
      let () = register_dynamic := false
      let () = register_common string X.name get set clear is_set
    end

    module StringSet(X: sig val name: string end) = struct
      let () = register_dynamic := true
      include StringSet(X)
      let () = register_dynamic := false
      let () =
	register_common Kernel_type.string_set X.name get set clear is_set;
	(*register STRING_SET functions *)
	FunTbl.register tbl (X.name ^ ".add") (func string unit) add;
	FunTbl.register tbl (X.name ^ ".add_set") (func string unit) add_set;
	FunTbl.register tbl (X.name ^ ".is_empty") (func unit bool) is_empty;
	FunTbl.register tbl (X.name ^ ".iter") (func (func string unit) unit) iter
    end

  end

  module Apply = struct

    module type Common = sig
      type t
      val get: string -> t
      val set: string -> t  -> unit
      val clear: string -> unit -> unit
      val is_set: string  -> bool
    end

    module Common (X: sig type common val sign : common Type.t end ) = struct
      type t = X.common
      let sign = X.sign
      let get name = FunTbl.apply tbl (name^".get") (func unit sign) ()
      let set name = FunTbl.apply tbl (name^".set") (func sign unit)
      let clear name = FunTbl.apply tbl (name^".clear") (func unit unit)
      let is_set name = FunTbl.apply tbl (name^".is_set") (func unit bool) ()
    end

    module Bool = struct
     include Common (struct type common= bool let sign = bool end )
      let on name = FunTbl.apply tbl (name^".on")  (func unit unit)
      let off name = FunTbl.apply tbl (name^".off") (func unit unit)
    end

    module Int = struct
      include Common (struct type common= int let sign = int end )
      let incr name =  FunTbl.apply tbl (name^".incr") (func unit unit)
    end

    module String = Common (struct type common= string let sign = string end )

    module StringSet = struct
      include Common (struct
			type common = Cilutil.StringSet.t
			let sign = Kernel_type.string_set
		      end )
      let add name = FunTbl.apply tbl (name^".add") (func string unit)
      let add_set name = FunTbl.apply tbl (name^".add_set") (func string unit)
      let is_empty name = FunTbl.apply tbl (name^".is_empty") (func unit bool) ()
      let iter name = FunTbl.apply tbl (name^".iter") (func (func string unit) unit)
      let fold _name = assert false
    end

  end

  module Debug = Zero(struct let name = "Dynamic.Debug" end)
  module AddPath = StringSet(struct let name = "Dynamic.AddPath" end)
  module LoadModule = StringSet(struct let name = "Dynamic.LoadModule" end)

end

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

module Debug = Zero(struct let name = "Debug" end)
module Quiet = False(struct let name = "Quiet" end)

module UseUnicode = struct
  include True(struct let name = "UseUnicode" end)
  let set b =
    set b;
    Cil.print_utf8 := b
  let on () = set true
  let off () = set false
end

module Obfuscate = False(struct let name = "Obfuscate" end)

module Metrics = struct
  module Print = False(struct let name = "Metrics.Print" end)
  module Dump =
    EmptyString(struct let name = "Metrics.Dump" end)
  let is_on () = Print.is_set () || Dump.is_set ()
end

module WarnUnspecifiedOrder =
  False(struct let name = "WarnUnspecifiedOrder" end)

module ForceDeps = False(struct let name = "ForceDeps" end)
module ForceCallDeps = False(struct let name = "ForceCallDeps" end)
module ForceUsers = False(struct let name = "ForceUsers" end)
module ForceMemzones = False(struct let name = "ForceMemzones" end)
module ForceValues = False(struct let name = "ForceValues" end)
module ForceOut = False(struct let name = "ForceOut" end)
module ForceInput = False(struct let name = "ForceInput" end)
module ForceInputWithFormals =
  False(struct let name = "ForceInputWithFormals" end)
module ForceInout = False(struct let name = "ForceInout" end)
module ForceDeref = False(struct let name = "ForceDeref" end)
module ForceAccessPath =
  False(struct let name = "ForceAccessPath" end)
module UnsafeArrays = False(struct let name = "UnsafeArrays" end)

module WideningLevel =
  Int(struct let default = 3 let name = "WideningLevel" end)
module UnrollingLevel = Zero(struct let name = "UnrollingLevel" end)
module SemanticUnrollingLevel =
  Zero(struct let name = "SemanticUnrollingLevel" end)
module ArrayPrecisionLevel =
  Int(struct let default = 200 let name = "ArrayPrecisionLevel" end)

module MainFunction = String(struct let default = "main" let name = "MainFunction" end)

module LibEntry = Bool(struct let name = "LibEntry" let default = false end)

module Machdep = struct
  include EmptyString(struct let name = "Machdep" end)
  let () = Project.Computation.add_dependency Cil.selfMachine self
end

module PrintCode = False(struct let name = "PrintCode" end)
module PrintComments = False(struct let name = "PrintComments" end)
module CodeOutput = struct

  include EmptyString(struct let name = "CodeOutput" end)

  let state = ref (Format.std_formatter, stdout)
  let get_fmt () = fst !state

  let close () =
    let fmt, cout = !state in
    Format.pp_print_flush fmt ();
    if get () <> "" then close_out cout

  let () = at_exit close

  (* overide [set] *)
  let set out_cmdline =
    set out_cmdline;
    close ();
    let new_state =
      if out_cmdline = "" then
	Format.std_formatter, stdout
      else
	try
	  let out = open_out out_cmdline in
          let fmt = Format.formatter_of_out_channel out in
          fmt, out
	with Sys_error s ->
          Format.printf
            "Warning: could not open %s for code output:\n%s.\n\
I will output the code on stdout instead@."
            out_cmdline s;
          Format.std_formatter, stdout
    in
    state := new_state

  let unsafe_set _ = assert false
  let clear () = set ""

  let () =
    Project.register_after_set_current_hook ~user_only:false
      (fun () -> (* set [state] to the right value *) set (get ()))

end

module PrintVersion = False(struct let name = "PrintVersion" end)
module PrintShare = False(struct let name = "PrintShare" end)
module Time = EmptyString(struct let name = "Time" end)

module SaveState = EmptyString(struct let name = "SaveState" end)
module LoadState = EmptyString(struct let name = "LoadState" end)

module CallgraphFilename =
  EmptyString(struct let name = "CallgraphFilename" end)
module CallgraphInitFunc =
  StringSet(struct let name = "CallgraphInitFunc" end)
module Semantic_Callgraph= struct
  module Dump = False(struct let name = "Semantic_Callgraph.Dump" end)
end

module AlcoolPrintLocations =
  False(struct let name = "AlcoolPrintLocations" end)
module AlcoolExtraction =
  EmptyString(struct let name = "AlcoolExtraction" end)
module PointersExtraction =
  EmptyString(struct let name = "PointersExtraction" end)
module XmlPointersExtraction =
  EmptyString(struct let name = "XmlPointersExtraction" end)
module RawExtraction =
  EmptyString(struct let name = "RawExtraction" end)
module PromelaExtraction =
  EmptyString(struct let name = "PromelaExtraction" end)
module Relevant = False(struct let name = "Relevant" end)
module Report = EmptyString(struct let name = "Report" end)
module ConstFuncArrays =
  False(struct let name = "ConstFuncArrays" end)
module IgnoreOverflow = False(struct let name = "IgnoreOverflow" end)
module IgnoreUnspecified = False(struct let name = "IgnoreUnspecified" end)
module MemFunctions = StringSet(struct let name = "MemFunctions" end)
module WidenVariables =
  StringSet(struct let name = "WidenVariables" end)
module ReadAnnot = True(struct let name = "ReadAnnot" end)
module PreprocessAnnot =
  False(struct let name = "PreprocessAnnot" end)
module CppCommand =
  EmptyString(struct let name = "CppCommand" end)
module CppExtraArgs =
  StringSet(struct let name = "CppExtraArgs" end)
module Wp = struct
  module Cfg = False(struct let name = "Wp.Cfg" end)
  module Post = False(struct let name = "Wp.Post" end)
  module Debug = Zero(struct let name = "Wp.Debug" end)
end
module MielSpecFilename =
  EmptyString(struct let name = "MielSpecFilename" end)
module PropagateTop = False(struct let name = "PropagateTop" end)

module AutomaticContextMaxDepth =
  Int(struct let name = "AutomaticContextMaxDepth" let default = 2 end)

module AutomaticContextMaxWidth =
  Int(struct let name = "AutomaticContextMaxWidth" let default = 2 end)

module AllocatedContextValid =
  False(struct let name = "AllocatedContextValid" end)

module MemExecAll = False(struct let name = "MemExecAll" end)

module UseRelations = True(struct let name = "UseRelations" end)
  (* This can be set to false to debug value analysis without relations *)

module SimplifyCfg = False(struct let name = "SimplifyCfg" end)

module KeepSwitch = False(struct let name = "KeepSwitch" end)

module KeepOnlyLastRun =
  False(struct let name = "KeepOnlyLastRun" end)

module MemoryFootprint = struct
  include Int(struct let name = "MemoryFootprint" let default = 2 end)
  let set x =
    if not (equal x (get ())) then begin
      Binary_cache.MemoryFootprint.set x;
      Buckx.MemoryFootprint.set x;
      set x
    end
end

module FloatDigits =
  Int(struct let name = "FloatDigits" let default = 4 end)

module Files = struct
  include StringList(struct let name = "Files" end)
  module Check = False(struct let name = "Debug.Check" end)
  module Copy = False(struct let name = "Debug.Copy" end)
  module Orig_name = False(struct let name = "Debug.Orig_name" end)
end

(** All absolute address are invalid *)
module MinValidAbsoluteAddress =
  Build(struct
	  type t = Abstract_interp.Int.t
	  let default () = Abstract_interp.Int.zero
	  let name = "MinValidAbsoluteAddress"
	  let equal = Abstract_interp.Int.equal
	  let fun_ty = Kernel_type.big_int
	end)

module MaxValidAbsoluteAddress =
  Build(struct
	  type t = Abstract_interp.Int.t
	  let default () = Abstract_interp.Int.minus_one
	  let name = "MaxValidAbsoluteAddress"
	  let equal = Abstract_interp.Int.equal
	  let fun_ty = Kernel_type.big_int
	end)

(** {3 Viewer options} *)

module MonospaceFontName =
  String(struct
	   let default = "Monospace,Lucida Sans Unicode,Sans"
	   let name = "MonospaceFontName"
	 end)

module GeneralFontName =
  String(struct
	   let default = "Helvetica,Lucida Sans Unicode,Sans"
	   let name = "GeneralFontName"
	 end)

(** {3 Security options} *)

module Security = struct

  module Analysis = False(struct let name = "Security.Analysis" end)
  module Lattice =
    String(struct
	     let name = "Security.Lattice"
	     let default = "weak"
	   end)
  module PropagateAssertions =
    False(struct let name = "Security.PropagateAssertions" end)
  module Slicing = False(struct let name = "Security.Slicing" end)

  let is_on () = Analysis.get () || Slicing.get ()

  module LogicAnnotation =
    EmptyString(struct let name = "Security.LogicAnnotation" end)

  module Debug = Zero(struct let name = "Security.Debug" end)

  let get_selection_after_slicing () =
    let add s = Project.Selection.add s Kind.Do_Not_Select_Dependencies in
    (add Analysis.self $ add Lattice.self $ add PropagateAssertions.self
       $ add LogicAnnotation.self)
      (Project.Selection.singleton Debug.self Kind.Do_Not_Select_Dependencies)


end

module Impact = struct
  module Pragma = StringSet(struct let name = "Impact.Pragma" end)
  module Print = False(struct let name = "Impact.Print" end)
  module Slicing = False(struct let name = "Impact.Slicing" end)
  let is_on () = not (Pragma.is_empty ())
end

(** {3 Jessie options} *)

module Jessie = struct
  module ProjectName = EmptyString(struct let name = "Jessie.ProjectName" end)
  module Behavior = EmptyString(struct let name = "Jessie.Behavior" end)
  module Analysis = False(struct let name = "Jessie.Analysis" end)
  module Gui = False(struct let name = "Jessie.Gui" end)
  module JcOpt = StringSet(struct let name = "Jessie.JcOpt" end)
  module WhyOpt = StringSet(struct let name = "Jessie.WhyOpt" end)

  type int_model = IMexact | IMbounded | IMmodulo
  let int_model = Type.make "int_model" IMmodulo
  let () =
    Journal.register_printer
      int_model
      (fun fmt im ->
	 Format.fprintf fmt "%s"
	   (match im with
	    | IMexact -> "IMexact"
	    | IMbounded -> "IMbounded"
	    | IMmodulo -> "IMmodulo"))

  module IntModel =
    IndexedVal(
      struct
	type t = int_model
	let default_val = IMbounded
	let default_key = "bounded"
	let name = "Jessie.IntModel"
	let fun_ty = int_model
      end)
  let () = IntModel.add_choice "exact" IMexact
  let () = IntModel.add_choice "modulo" IMmodulo
  module GenOnly = False(struct let name = "Jessie.GenOnly" end)
  module GenGoals = False(struct let name = "Jessie.GenGoals" end)
  module SepRegions = True(struct let name = "Jessie.SepRegions" end)
  module StdStubs = False(struct let name = "Jessie.StdStubs" end)
  module InferAnnot = EmptyString(struct let name = "Jessie.InferAnnot" end)
  module CpuLimit = Zero(struct let name = "Jessie.CpuLimit" end)
  module AbsDomain =
    String(struct
	     let name = "Jessie.AbsDomain"
	     let default = "poly"
	   end)
  module Atp =
    String(struct
	     let name = "Jessie.Atp"
	     let default = "simplify"
	   end)
  module HintLevel = Zero(struct let name = "Jessie.InsertHints" end)
end

(** {3 PDG options} *)

module Pdg = struct
  module BuildAll = False(struct let name = "Pdg.BuildAll" end)
  module BuildFct = StringSet(struct let name = "Pdg.BuildFct" end)
  module PrintBw = False(struct let name = "Pdg.PrintBw" end)
  module DotBasename =
    EmptyString(struct let name = "Pdg.DotBasename" end)
  module DotPostdomBasename =
    EmptyString(struct let name = "Pdg.DotPostdomBasename" end)
  module Verbosity =
    Int (struct let name = "Pdg.Verbosity" let default = 0 end)
end

(** {2 Sparecode options} *)

module Sparecode = struct
  module Analysis = False(struct let name = "Sparecode.Analysis" end)
  module NoAnnot = False(struct let name = "Sparecode.NoAnnot" end)
  module GlobDecl = False(struct let name = "Sparecode.GlobDecl" end)
end


(** {3 Slicing options} *)

module Slicing = struct
  module Select = struct
    module Calls =  StringSet(struct let name = "Slicing.Select.Calls" end)
    module Return = StringSet(struct let name = "Slicing.Select.Return" end)
    module Threat = StringSet(struct let name = "Slicing.Select.Threat" end)
    module Assert = StringSet(struct let name = "Slicing.Select.Assert" end)
    module LoopInv =
      StringSet(struct let name = "Slicing.Select.LoopInv" end)
    module LoopVar =
      StringSet(struct let name = "Slicing.Select.LoopVar" end)
    module Pragma = StringSet(struct let name = "Slicing.Select.Pragma" end)
    module RdAccess =
      StringSet(struct let name = "Slicing.Select.RdAccess" end)
    module WrAccess =
      StringSet(struct let name = "Slicing.Select.WrAccess" end)
    module Value = StringSet(struct let name = "Slicing.Select.Value" end)
  end
  module Mode = struct
    module Verbose = Zero(struct let name = "Slicing.Mode.Verbosity" end)
    module Callers = True(struct let name = "Slicing.Mode.Callers" end)
    module Calls =
      Int(struct let name = "Slicing.Mode.Calls" let default = 2 end)
    module SliceUndef =
      False(struct let name = "Slicing.Mode.SliceUndef" end)
  end
  module Print = False(struct let name = "Slicing.Print" end)
  let is_on () =
    not (Select.Calls.is_empty ()
	 && Select.Return.is_empty ()
	 && Select.Threat.is_empty ()
	 && Select.Assert.is_empty ()
	 && Select.LoopInv.is_empty ()
	 && Select.LoopVar.is_empty ()
	 && Select.Pragma.is_empty ()
	 && Select.RdAccess.is_empty ()
	 && Select.WrAccess.is_empty ()
	 && Select.Value.is_empty ())
end

(** {3 Constant propagation options} *)

module Constfold = False(struct let name = "Constfold" end)
module Constant_Propagation = struct
  module SemanticConstFolding =
    False
      (struct
         let name = "Constant_Propagation.SemanticConstFolding"
       end)
  module SemanticConstFold =
    StringSet
      (struct let name = "Constant_Propagation.SemanticConstFold" end)
  module CastIntro =
    False(struct let name = "Constant_Propagation.CastIntro" end)
end

(** {3 Cxx options} *)

module Unmangling: INDEXED_VAL with type value = string -> string =
  IndexedVal(
    struct
      type t = string->string
      let default_val = fun x -> x
      let default_key = "id"
      let name = "Unmangling"
      let fun_ty = Type.func Type.string Type.string
    end)

module PrintCxx = False(struct let name = "PrintCxx" end)

(** {3 Occurrence} *)

module Occurrence = struct
  module Debug = Zero(struct let name = "Occurrence.Debug" end)
  module Print = False(struct let name = "Occurrence.Print" end)
end

(** {3 Journalization] *)
module Journal = struct
  module Disable = False(struct let name = "Journal.Disable" end)
  module Name = struct
    include String
      (struct
	 let name = "Journal.Name"
	 let default = Journal.get_name ()
       end)
  end
end

(** {2 Options which define context of analyses } *)

let get_selection_context () =
  let a o = Project.Selection.add o Kind.Do_Not_Select_Dependencies in
  let has_dependencies o =
    try
      Project.Selection.iter
	(fun _ -> raise Exit)
	(Project.Selection.singleton o Kind.Only_Select_Dependencies);
      false
    with Exit ->
      true
  in
  (* automatically select all options which have some dependencies:
     they have an impact of some analysis. *)
  let sel_ctx =
    Project.Selection.fold
      (fun o _ acc -> if has_dependencies o then a o acc else acc)
      (get_selection ())
      Project.Selection.empty
  in
  (* Value analysis *)
  let sel_ctx = a WideningLevel.self sel_ctx in
  let sel_ctx = a FloatDigits.self sel_ctx in
  (* General options *)
  ((a Dynamic.Debug.self) $
   (a Dynamic.AddPath.self) $
   (a Dynamic.LoadModule.self) $
   (a UseUnicode.self) $
   (a Quiet.self) $
   (a Debug.self) $
   (a Journal.Disable.self) $
   (a Files.Check.self) $
   (a Files.Copy.self))
    sel_ctx


(** Aorai plug-in (ltl_to_acsl) *)
module Ltl_to_acsl = struct
  module Analysis =  False(struct let name = "Cmdline.Ltl_to_acsl.Analysis If true, then the Aorai plug-in is used" end)
  module Ltl_File =  EmptyString(struct let name = "Cmdline.Ltl_to_acsl.Ltl_File File containing LTL property" end)
  module OnlyToLTL = False(struct let name = "Cmdline.Ltl_to_acsl.OnlyToLTL" end)
  module Promela_File = EmptyString(struct let name = "Cmdline.Ltl_to_acsl.Promela_File File into which the LTL formula has to be generated" end)
  module Output_Spec = False(struct let name = "Cmdline.Ltl_to_acsl.Ltl_Spec" end)
  module Verbose = False(struct let name = "Cmdline.Ltl_to_acsl.Verbose" end)
  module Output_C_File = EmptyString(struct let name = "Cmdline.Ltl_to_acsl.Ltl_C_File Annotated C code generated" end)
  module OnlyFromPromela = False(struct let name = "Cmdline.Ltl_to_acsl.OnlyFromPromela" end)
  module Dot = False(struct let name = "Cmdline.Ltl_to_acsl.Dot" end)

  module AbstractInterpretation = False(struct let name = "Cmdline.Ltl_to_acsl.AbstractInterpretation" end)
  module AdvanceAbstractInterpretation  = False(struct let name = "Cmdline.Ltl_to_acsl.AdvanceAbstractInterpretation" end)
end


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
