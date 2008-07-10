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

(* $Id: cmdline.ml,v 1.177 2008/07/04 09:34:09 uid524 Exp $ *)

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
end

type 'a option_accessor = {get : unit -> 'a ; set : 'a -> unit }

type kind =
  | Bool of bool option_accessor
  | Int of int option_accessor
  | String of string option_accessor
  | StringSet of string option_accessor


type t = kind * string

let options : t list ref = ref []
let iter_on_options f = List.iter f !options

module Build
  (X:sig
     type t
     val default: t
     val name: string
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
       let name = Project.Computation.Name.make X.name
       let dependencies = []
     end)

  type t = data

  let () = add_option select

  let is_set () = not (X.equal X.default (get ()))

  let set x =
    if not (X.equal x (get ())) then begin
      set x;
      if is_set () then Selected_Options.extend (fun () -> set x)
    end

  let clear () = set X.default
  let equal = X.equal

end

(** {3 Boolean} *)

module type BOOL = sig
  include S with type t = bool
  val on: unit -> unit
  val off: unit -> unit
end

module Bool(X:sig val default: bool val name: string end) = struct
  include Build(struct type t = bool include X let equal = (=) end)
  let on () = set true
  let off () = set false
  let () =
    options := 
      (Bool{get=get;set=set}, Project.Computation.Name.get name) :: !options
end

(** Build a boolean option initialized to [false]. *)
module False(X: sig val name: string end) =
  Bool(struct include X let default = false end)

(** Build a boolean option initialized to [true]. *)
module True(X: sig val name: string end) =
  Bool(struct include X let default = true end)

(** {3 Integer} *)

module type INT = sig
  include S with type t = int
  val incr: unit -> unit
end

(** Build an integer option. *)
module Int(X: sig val default: int val name: string end) = struct
  include Build(struct type t = int include X let equal = (=) end)
  let incr () = set (succ (get ()))
  let () =
    options := 
      (Int{get=get;set=set}, Project.Computation.Name.get name) :: !options
end

(** Build an integer option initialized to [0]. *)
module Zero(X: sig val name: string end) =
  Int(struct include X let default = 0 end)

(** {3 String} *)

module type STRING = S with type t = string

(** Build a string option. *)
module String(X: sig val default: string val name: string end) = struct 
  include Build(struct type t = string include X let equal = (=) end)
  let () =
    options := 
      (String{get=get;set=set}, Project.Computation.Name.get name) :: !options
end

(** Build a string option initialized to [""]. *)
module EmptyString(X: sig val name: string end) =
  String(struct include X let default = "" end)

(** {3 String set} *)

module type STRING_SET = sig
  include S with type t = Cilutil.StringSet.t
  val set_set: string -> unit

  val add: string -> unit
  val remove: string -> unit
  val add_set: string -> unit
  val remove_set: string -> unit
  val is_empty: unit -> bool
  val iter: (string -> unit) -> unit
end

(** Build an option as a set of strings, initialized to the empty set. *)
module StringSet(X: sig val name: string end) = struct
  open Cilutil
  include Build(struct
		  type t = StringSet.t
		  let default = StringSet.empty
		  let name = X.name
		  let equal = StringSet.equal
		end)
  let add x = set (StringSet.add x (get ()))
  let remove x = set (StringSet.remove x (get()))

  let split_set = Str.split (Str.regexp "[ \t]*,[ \t]*")

  let set_and_split x acc = set (List.fold_right StringSet.add (split_set x) acc)
  let set_set x = set_and_split x StringSet.empty
  let add_set x = set_and_split x (get ())

  let get_set () = 
    Cilutil.StringSet.fold (fun s acc -> if acc<>"" then s^", "^acc else s) (get ()) ""

  let remove_set x =
    set (List.fold_right StringSet.remove (split_set x) (get()))
  let is_empty () = StringSet.is_empty (get ())
  let iter f = StringSet.iter f (get ())

  let () =
    options := (StringSet{get=get_set;set=set_set}, Project.Computation.Name.get name) :: !options

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
end

module IndexedVal (V:COMPLEX_VALUE):INDEXED_VAL with type value = V.t = struct

  type value = V.t

  let options = Hashtbl.create 13
  let add_choice k v  = Hashtbl.add options k v
  let () = add_choice V.default_key V.default_val

  let create () = ref V.default_key

  let curr_choice = ref (create())

  module StateAux =
  struct
    let name = Project.Computation.Name.make V.name
    let create = create

    type t = string ref

    let before_load () = ()
    let after_load () = ()
    let get () = !curr_choice
    let set s =
      if s <> get () then
	if Hashtbl.mem options !s then
          curr_choice:=s
	else
          Printf.eprintf
            "Warning: %s: identifier %s is not a valid index for this option. \
         Option is unchanged.\n" V.name !s

    let dependencies = []
    let copy s = ref (!s)
    let rehash s = copy s
    let clear tbl = tbl:= V.default_key
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

end

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

module Debug = Zero(struct let name = "Cmdline.debug" end)
module Quiet = False(struct let name = "Cmdline.quiet" end)

module UseUnicode = struct
  include True(struct let name = "Cmdline.use unicode" end)
  let set b =
    set b;
    Cil.print_utf8 := b
  let on () = set true
  let off () = set false
end

module Obfuscate = False(struct let name = "Cmdline.obfuscate" end)

module Metrics = struct
  module Print = False(struct let name = "pretty print metrics on stdout" end)
  module Dump = 
    EmptyString(struct let name = "pretty print metrics in a file" end)
  let is_on () = Print.is_set () || Dump.is_set ()
end

module ForceDeps = False(struct let name = "Cmdline.force deps" end)
module ForceCallDeps = False(struct let name = "Cmdline.force call deps" end)
module ForceUsers = False(struct let name = "Cmdline.force users" end)
module ForceMemzones = False(struct let name = "Cmdline.force memzones" end)
module ForceValues = False(struct let name = "Cmdline.force values" end)
module ForceOut = False(struct let name = "Cmdline.force out" end)
module ForceInput = False(struct let name = "Cmdline.force input" end)
module ForceInout = False(struct let name = "Cmdline.force inout" end)
module ForceDeref = False(struct let name = "Cmdline.force deref" end)
module ForceAccessPath =
  False(struct let name = "Cmdline.force access path" end)
module UnsafeArrays = False(struct let name = "Cmdline.unsafe arrays" end)

module WideningLevel =
  Int(struct let default = 3 let name = "Cmdline.widening level" end)
module UnrollingLevel = Zero(struct let name = "Cmdline.unrolling level" end)
module SemanticUnrollingLevel =
  Zero(struct let name = "Cmdline.SemanticUnrollingLevel" end)
module ArrayPrecisionLevel =
  Int(struct let default = 200 let name = "Cmdline.ArrayPrecisionLevel" end)

module MainFunction = struct
  include
    String(struct let default = "main" let name = "Cmdline.MainFunction" end)
  let unsafe_set = set
  let set x =
    if not (equal x (get ())) then begin
      set x;
      Project.clear
	~only:(Project.Selection.singleton self Kind.Only_Select_Dependencies)
	()
    end
(*  let () = depend Cil_state.self *)
end

module LibEntry = struct
  include Bool(struct let name = "Cmdline.LibEntry" let default = false end)
  let unsafe_set = set
  let set x =
    if not (equal x (get ())) then begin
      set x;
      Project.clear
	~only:(Project.Selection.singleton self Kind.Only_Select_Dependencies)
	()
    end
(*  let () = depend Cil_state.self *)
end

module PrintCode = False(struct let name = "Cmdline.PrintCode" end)
module PrintComments = False(struct let name = "Cmdline.PrintComments" end)
module CodeOutput = EmptyString(struct let name = "Cmdline.CodeOutput" end)

module PrintVersion = False(struct let name = "Cmdline.PrintVersion" end)
module Time = EmptyString(struct let name = "Cmdline.Time" end)
module Machdep = EmptyString(struct let name = "Cmdline.Machdep" end)
module SaveState = EmptyString(struct let name = "Cmdline.SaveState" end)
module LoadState = EmptyString(struct let name = "Cmdline.LoadState" end)

module CallgraphFilename =
  EmptyString(struct let name = "Cmdline.callgraph filename" end)
module CallgraphInitFunc =
  StringSet(struct let name = "Cmdline.callgraph init func" end)
module AlcoolPrintLocations =
  False(struct let name = "Cmdline.alcool print locations" end)
module AlcoolExtraction =
  EmptyString(struct let name = "Cmdline.alcool extraction" end)
module PointersExtraction =
  EmptyString(struct let name = "Cmdline.pointers extraction" end)
module XmlPointersExtraction =
  EmptyString(struct let name = "Cmdline.XML pointers extraction" end)
module RawExtraction =
  EmptyString(struct let name = "Cmdline.raw extraction" end)
module PromelaExtraction =
  EmptyString(struct let name = "Cmdline.promela extraction" end)
module Relevant = False(struct let name = "Cmdline.relevant" end)
module Report = EmptyString(struct let name = "Cmdline.report" end)
module ConstFuncArrays =
  False(struct let name = "Cmdline.Const func arrays" end)
module IgnoreOverflow = False(struct let name = "Cmdline.ignore overflow" end)
module MemFunctions = StringSet(struct let name = "Cmdline.mem functions" end)
module WidenVariables =
  StringSet(struct let name = "Cmdline.widen variables" end)
module ReadAnnot = True(struct let name = "read annotations" end)
module PreprocessAnnot =
  False(struct let name = "preprocess annotations" end)
module CppCommand =
  EmptyString(struct let name = "cpp command" end)
module CppExtraArgs =
  EmptyString(struct let name = "cpp extra arguments" end)
module WpCfg = False(struct let name = "Cmdline.wp cfg" end)
module MielSpecFilename =
  EmptyString(struct let name = "Cmdline.miel spec filename" end)
module PropagateTop = False(struct let name = "Cmdline.propagate top" end)
module LeafFuncReturnInt =
  False(struct let name = "Cmdline.leaf func return int" end)

module AutomaticContextMaxDepth =
  Int(struct let name = "Cmdline.AutomaticContextMaxDepth" let default = 2 end)

module AutomaticContextMaxWidth =
  Int(struct let name = "Cmdline.AutomaticContextMaxWidth" let default = 2 end)

module AllocatedContextValid =
  False(struct let name = "Cmdline.allocated contex valid" end)

module MemExecAll = False(struct let name = "Cmdline.mem exec all" end)

module UseRelations = True(struct let name = "Cmdline.use relations" end)
  (* This can be set to false to debug value analysis without relations *)

module SimplifyCfg = False(struct let name = "Cmdline.simplify cfg" end)

module KeepSwitch = False(struct let name = "Cmdline.keep switch" end)

module KeepOnlyLastRun =
  False(struct let name = "Cmdline.keep only last run" end)

module MemoryFootprint = struct
  include Int(struct let name = "Cmdline.memory footprint" let default = 2 end)
  let set x =
    if not (equal x (get ())) then begin
      Binary_cache.MemoryFootprint.set x;
      Buckx.MemoryFootprint.set x;
      set x
    end
end

module FloatDigits =
  Int(struct let name = "Cmdline.float digits" let default = 4 end)

module Files = struct
  module M =
  Build(struct
	  type t = string list
	  let name = "Cmdline.Files"
	  let default = []
	  let equal = (=)
	end)
  include M
  module Check =
    False
      (struct
         let name = "Debug option: performs various integrity checks on the AST"
       end)
  module Copy =
    False
      (struct
         let name = "Debug option: makes a copy of the initial \
                     state before starting the real analyses"
       end)
end

(** All absolute address are invalid *)
module MinValidAbsoluteAddress =
  Build(struct
	  type t = Abstract_interp.Int.t
	  let default = Abstract_interp.Int.one
	  let name = "Cmdline.min valid absolute address"
	  let equal = Abstract_interp.Int.equal
	end)

module MaxValidAbsoluteAddress =
  Build(struct
	  type t = Abstract_interp.Int.t
	  let default = Abstract_interp.Int.zero
	  let name = "Cmdline.max valid absolute address"
	  let equal = Abstract_interp.Int.equal
	end)

(** {3 Viewer options} *)

module MonospaceFontName =
  String(struct
	   let default = "Monospace 12"
	   let name = "Cmdline.MonospaceFontName"
	 end)

module GeneralFontName =
  String(struct
	   let default = "Helvetica,Lucida Sans Unicode,Sans 10"
	   let name = "Cmdline.GeneralFontName"
	 end)

(** {3 Security options} *)

module Security = struct

  module Analysis = False(struct let name = "Cmdline.Security.Analysis" end)
  module Lattice =
    String(struct
	     let name = "Cmdline.Security.Lattice"
	     let default = "weak"
	   end)
  module PropagateAssertions =
    False(struct let name = "Cmdline.Security.PropagateAssertions" end)
  module Slicing = False(struct let name = "Cmdline.Security.Slicing" end)

  let is_on () = Analysis.get () || Slicing.get ()

  module LogicAnnotation =
    EmptyString(struct let name = "Cmdline.Security.LogicAnnotation" end)

  module Debug = Zero(struct let name = "Cmdline.Security.Debug" end)

  let get_selection_after_slicing () =
    let add s = Project.Selection.add s Kind.Do_Not_Select_Dependencies in
    (add Analysis.self $ add Lattice.self $ add PropagateAssertions.self
       $ add LogicAnnotation.self)
      (Project.Selection.singleton Debug.self Kind.Do_Not_Select_Dependencies)


end

module Impact = struct
  module Pragma = StringSet(struct let name = "Cmdline.Impact.Pragma" end)
  module Print = False(struct let name = "Cmdline.Impact.Print" end)
  module Slicing = False(struct let name = "Cmdline.Impact.Slicing" end)
  let is_on () = not (Pragma.is_empty ())
end

(** {3 Jessie options} *)

module Jessie = struct
  module ProjectName = EmptyString(struct let name = "Cmdline.jessie project name" end)
  module Analysis = False(struct let name = "Cmdline.jessie analysis" end)
  module Gui = False(struct let name = "Cmdline.jessie gui" end)
  module JcOpt = StringSet(struct let name = "Cmdline.jessie jc opt" end)
  module WhyOpt = StringSet(struct let name = "Cmdline.jessie why opt" end)
  type int_model = IMexact | IMbounded | IMmodulo
  module IntModel =
    IndexedVal(
      struct
	type t = int_model
	let default_val = IMbounded
	let default_key = "bounded"
	let name = "Cmdline.jessie int model"
      end)
  let () = IntModel.add_choice "exact" IMexact
  let () = IntModel.add_choice "modulo" IMmodulo
  module GenOnly = False(struct let name = "Cmdline.jessie gen only" end)
end

(** {3 PDG options} *)

module Pdg = struct
  module BuildAll = False(struct let name = "Cmdline.build pdg" end)
  module BuildFct = StringSet(struct let name = "Cmdline.build fct pdg" end)
  module PrintBw = False(struct let name = "Cmdline.pdg print codpds" end)
  module DotBasename =
    EmptyString(struct let name = "Cmdline.pdg dot basename" end)
  module DotPostdomBasename =
    EmptyString(struct let name = "Cmdline.pdg postdominators dot basename" end)
  module Verbosity =
    Int (struct let name = "Pdg verbosity level" let default = 0 end)
end

(** {2 Sparecode options} *)

module Sparecode = struct
  module Analysis = False(struct let name = "Cmdline.sparecode analysis" end)
  module NoAnnot = False(struct let name = "Cmdline.sparecode no annot" end)
end


(** {3 Slicing options} *)

module Slicing = struct
  module Select = struct
    module Calls =  StringSet(struct let name = "Cmdline.slice calls" end)
    module Return = StringSet(struct let name = "Cmdline.slice return" end)
    module Threat = StringSet(struct let name = "Cmdline.slice threat" end)
    module Assert = StringSet(struct let name = "Cmdline.slice assert" end)
    module LoopInv =
      StringSet(struct let name = "Cmdline.slice loop invariant" end)
    module LoopVar =
      StringSet(struct let name = "Cmdline.slice loop variant" end)
    module Pragma = StringSet(struct let name = "Cmdline.slice pragma" end)
    module RdAccess =
      StringSet(struct let name = "Cmdline.slice read access" end)
    module WrAccess =
      StringSet(struct let name = "Cmdline.slice write access" end)
    module Value = StringSet(struct let name = "Cmdline.slice value" end)
  end
  module Mode = struct
    module Verbose = Zero(struct let name = "Cmdline.slice verbosity" end)
    module Callers = True(struct let name = "Cmdline.slice callers" end)
    module Calls =
      Int(struct let name = "Cmdline.how to slice calls" let default = 2 end)
    module SliceUndef =
      False(struct let name = "Cmdline.slice undefined functions" end)
  end
  module Print = False(struct let name = "Cmdline.print sliced code" end)
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

module Constfold = False(struct let name = "Cmdline.constfold" end)
module Constant_Propagation = struct
  module SemanticConstFolding =
    False
      (struct
         let name = "Cmdline.Constant_Propagation.SemanticConstFolding"
       end)
  module SemanticConstFold =
    StringSet
      (struct let name = "Cmdline.Constant_Propagation.SemanticConstFold" end)
  module CastIntro =
    False(struct let name = "Cmdline.Constant_Propagation.CastIntro" end)
end

(** {3 Cxx options} *)

module Unmangling: INDEXED_VAL with type value = string -> string =
  IndexedVal(
    struct
      type t = string->string
      let default_val = fun x -> x
      let default_key = "id"
      let name = "Cmdline.unmangling option"
    end)

module PrintCxx = False(struct let name = "pretty-print cxx files" end)

(** {3 Occurrence} *)

module Occurrence = struct
  module Debug = Zero(struct let name = "Occurrence.Debug" end)
  module Print = False(struct let name = "Occurrence.Print" end)
end

(** {2 Options which define context of analyses } *)

let get_selection_context () =
  let a o = Project.Selection.add o Kind.Do_Not_Select_Dependencies in
  let sel_ctx = Project.Selection.empty in
  (* Value analysis *)
  let sel_ctx = a MinValidAbsoluteAddress.self sel_ctx in
  let sel_ctx = a MaxValidAbsoluteAddress.self sel_ctx in
  let sel_ctx = a AutomaticContextMaxDepth.self sel_ctx in
  let sel_ctx = a AllocatedContextValid.self sel_ctx in
  let sel_ctx = a IgnoreOverflow.self sel_ctx in
  let sel_ctx = a UnsafeArrays.self sel_ctx in
  (* General options *)
  let sel_ctx = a Machdep.self sel_ctx in
  let sel_ctx = a LibEntry.self sel_ctx in
  a MainFunction.self sel_ctx
  
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
