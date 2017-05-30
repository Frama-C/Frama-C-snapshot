(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Model Factory                                                      --- *)
(* -------------------------------------------------------------------------- *)

type mheap = Hoare | ZeroAlias | Typed of MemTyped.pointer
type mvar = Raw | Var | Ref | Caveat

type setup = {
  mvar : mvar ;
  mheap : mheap ;
  cint : Cint.model ;
  cfloat : Cfloat.model ;
}

(*[LC] All types in [model] must be Pervasives-comparable *)

type driver = LogicBuiltins.driver

(* -------------------------------------------------------------------------- *)
(* --- Description & Id                                                   --- *)
(* -------------------------------------------------------------------------- *)

let main (i,t) name =
  begin
    Buffer.add_string i name ;
    Buffer.add_string t (Transitioning.String.capitalize_ascii name) ;
  end

let add (i,t) part =
  begin
    Buffer.add_char i '_' ;
    Buffer.add_string i part ;
    Buffer.add_char t ' ' ;
    Buffer.add_char t '(' ;
    Buffer.add_string t (Transitioning.String.capitalize_ascii part) ;
    Buffer.add_char t ')' ;
  end

let descr_mtyped d = function
  | MemTyped.NoCast -> add d "nocast"
  | MemTyped.Unsafe -> add d "cast"
  | MemTyped.Fits -> ()

let descr_mheap d = function
  | ZeroAlias -> main d "zeroalias"
  | Hoare -> main d "hoare"
  | Typed p -> main d "typed" ; descr_mtyped d p

let descr_mvar d = function
  | Var -> ()
  | Ref -> add d "ref"
  | Raw -> add d "raw"
  | Caveat -> add d "caveat"

let descr_cint d = function
  | Cint.Machine -> ()
  | Cint.Natural -> add d "nat"

let descr_cfloat d = function
  | Cfloat.Real -> add d "real"
  | Cfloat.Float -> ()

let descr_setup (s:setup) =
  begin
    let i = Buffer.create 40 in
    let t = Buffer.create 40 in
    let d = (i,t) in
    descr_mheap d s.mheap ;
    descr_mvar d s.mvar ;
    descr_cint d s.cint ;
    descr_cfloat d s.cfloat ;
    ( Buffer.contents i , Buffer.contents t )
  end

let descriptions = Hashtbl.create 31 (*[LC] Not projectified: simple strings *)
let describe s =
  try Hashtbl.find descriptions s
  with Not_found -> let w = descr_setup s in Hashtbl.add descriptions s w ; w

(* -------------------------------------------------------------------------- *)
(* --- Generator & Model                                                  --- *)
(* -------------------------------------------------------------------------- *)

(* No implicit context for these AlterableVarUsage modules *) 
module type AlterableVarUsage = sig
  val datatype : string
  val param : Cil_types.varinfo -> MemVar.param
  val iter_inputs : ?kf:Kernel_function.t -> init:bool -> (Cil_types.varinfo -> unit) -> unit
end

module VarHoare : AlterableVarUsage =
struct
  let datatype = "Value"
  let param _x = MemVar.ByValue
  let iter_inputs ?kf ~init f =
    begin
      ignore init ;
      Globals.Vars.iter (fun x _initinfo -> f x) ;
      match kf with
      | None -> ()
      | Some kf -> List.iter f (Kernel_function.get_formals kf) ;
    end
end

module VarRefUsage =
struct

  let formal_ptr x =
    let open Cil_types in
    x.vformal && Cil.isPointerType x.vtype

  let param ~byref ~context x =
    let kf = Model.get_scope () in
    let init = match kf with
      | None -> false
      | Some f ->
          WpStrategy.is_main_init f ||
          Wp_parameters.InitAlias.get () ||
          ( WpStrategy.isInitConst () &&
            WpStrategy.isGlobalInitConst x ) in
    let open RefUsage in
    match RefUsage.get ?kf ~init x with
    | NoAccess -> MemVar.NotUsed
    | ByAddr -> MemVar.InHeap
    | ByRef when byref -> MemVar.ByRef
    | ByArray when context && formal_ptr x -> MemVar.InArray
    | ByValue when context && formal_ptr x -> MemVar.InContext
    | ByRef | ByArray | ByValue -> MemVar.ByValue

  let iter ?kf ~init f = RefUsage.iter ?kf ~init (fun x _usage -> f x)

end

module VarRef0 : AlterableVarUsage =
struct
  let datatype = "Ref0"
  let param = VarRefUsage.param ~byref:false ~context:false
  let iter_inputs = VarRefUsage.iter
end

module VarRef2 : AlterableVarUsage =
struct
  let datatype = "Ref2"
  let param = VarRefUsage.param ~byref:true ~context:false
  let iter_inputs = VarRefUsage.iter
end

module VarCaveat : AlterableVarUsage =
struct
  let datatype = "Caveat"
  let param = VarRefUsage.param ~byref:true ~context:true
  let iter_inputs = VarRefUsage.iter
end

module AltVarUsage(V : AlterableVarUsage) : MemVar.VarUsage =
struct
  let datatype = "VarUsage." ^ V.datatype

  let param x =
    let get_heap = Wp_parameters.InHeap.get in
    let get_ctxt = Wp_parameters.InCtxt.get in
    let get_refs = Wp_parameters.ByRef.get in
    let get_vars = Wp_parameters.ByValue.get in
    let open Cil_types in
    let module S = Datatype.String.Set in
    if S.mem x.vname (get_heap ()) then MemVar.InHeap else
    if S.mem x.vname (get_ctxt ()) then MemVar.InContext else
    if S.mem x.vname (get_refs ()) then MemVar.ByRef else
    if S.mem x.vname (get_vars ()) then MemVar.ByValue else
      V.param x

  (** A memory model context has to be set. *)
  let separation () =
    let kf = Model.get_scope () in
    let init = match kf with
      | None -> false
      | Some f -> WpStrategy.is_main_init f in
    let open Cil_types in
    let open MemVar in
    let r_mutex = ref [] in
    let r_other = ref [] in
    let s_mutex r = r_mutex := r::!r_mutex in
    let s_other r = r_other := r::!r_other in
    let s_partition x =
      if (Cil.isPointerType x.vtype) || (Cil.isArrayType x.vtype) then
        begin
          match param x with
          | ByValue   -> s_other (Separation.Arr x)
          | ByRef     -> s_mutex (Separation.Ptr x)
          | InHeap    -> s_other (Separation.Arr x)
          | InContext -> s_mutex (Separation.Ptr x)
          | InArray   -> s_mutex (Separation.Arr x)
          | NotUsed   -> ()
        end;
    in
    V.iter_inputs ?kf ~init
      (fun vi ->
         if vi.vglob then s_other (Separation.Var vi);
         if vi.vformal then s_partition vi;
      ) ;
    let open Separation in
    { mutex = List.rev !r_mutex ; other = List.rev !r_other }

end

module AltMake(V : AlterableVarUsage)(M : Memory.Model) =
  MemVar.Make( AltVarUsage(V) )(M)

module MHoareVar = AltMake(VarHoare)(MemEmpty)
module MHoareRef = AltMake(VarRef2)(MemEmpty)
module MTypedVar = AltMake(VarRef0)(MemTyped)
module MTypedRef = AltMake(VarRef2)(MemTyped)
module MCaveat = AltMake(VarCaveat)(MemTyped)

let memory mheap mvar : (module Memory.Model) =
  match mheap , mvar with
  | ZeroAlias , _     -> (module MemZeroAlias)
  | Hoare , (Raw|Var) -> (module MHoareVar)
  | Hoare ,   Ref     -> (module MHoareRef)
  | Typed _ , Raw     -> (module MemTyped)
  | Typed _ , Var     -> (module MTypedVar)
  | Typed _ , Ref     -> (module MTypedRef)
  | _    , Caveat     -> (module MCaveat)

(* -------------------------------------------------------------------------- *)
(* --- Tuning                                                             --- *)
(* -------------------------------------------------------------------------- *)

let configure_mheap = function
  | Hoare -> MemEmpty.configure ()
  | ZeroAlias -> MemZeroAlias.configure ()
  | Typed p -> MemTyped.configure () ; Context.set MemTyped.pointer p

let configure (s:setup) (d:driver) () =
  begin
    configure_mheap s.mheap ;
    Cint.configure s.cint ;
    Cfloat.configure s.cfloat ;
    Context.set LogicBuiltins.driver d ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Access                                                             --- *)
(* -------------------------------------------------------------------------- *)

module MODEL = FCMap.Make
    (struct
      type t = setup * driver
      let compare (s,d) (s',d') =
        let cmp = Pervasives.compare s s' in
        if cmp <> 0 then cmp else LogicBuiltins.compare d d'
    end)

let instances = ref (MODEL.empty : Model.t MODEL.t)

let instance (s:setup) (d:driver) =
  try MODEL.find (s,d) !instances
  with Not_found ->
    let id,descr = describe s in
    let module M = (val memory s.mheap s.mvar) in
    let tuning = [configure s d] in
    let separation kf = Model.on_scope (Some kf) M.separation () in
    let id,descr =
      if LogicBuiltins.is_default d then id,descr
      else
        ( id ^ "_" ^ LogicBuiltins.id d ,
          descr ^ " (Driver " ^ LogicBuiltins.descr d ^ ")" )
    in
    let model = Model.register ~id ~descr ~tuning ~separation () in
    instances := MODEL.add (s,d) model !instances ; model

let ident s = fst (describe s)
let descr s = snd (describe s)

let split ~warning (m:string) : string list =
  let tk = ref [] in
  let buffer = Buffer.create 32 in
  let flush () =
    if Buffer.length buffer > 0 then
      begin
        tk := !tk @ [Buffer.contents buffer] ;
        Buffer.clear buffer ;
      end
  in
  String.iter
    (fun c ->
       match c with
       | 'A' .. 'Z' -> Buffer.add_char buffer c
       | '_' | ',' | '@' | '+' | ' ' | '\t' | '\n' | '(' | ')' -> flush ()
       | _ -> warning (Printf.sprintf
                       "In model spec %S : unexpected character '%c'" m c)
    )
    (Transitioning.String.uppercase_ascii m) ;
  flush () ; !tk

let update_config ~warning m s = function
  | "ZEROALIAS" -> { s with mheap = ZeroAlias }
  | "HOARE" -> { s with mheap = Hoare }
  | "TYPED" -> { s with mheap = Typed MemTyped.Fits }
  | "CAST" -> { s with mheap = Typed MemTyped.Unsafe }
  | "NOCAST" -> { s with mheap = Typed MemTyped.NoCast }
  | "CAVEAT" -> { s with mvar = Caveat }
  | "RAW" -> { s with mvar = Raw }
  | "REF" -> { s with mvar = Ref }
  | "VAR" -> { s with mvar = Var }
  | "INT" | "CINT" -> { s with cint = Cint.Machine }
  | "NAT" -> { s with cint = Cint.Natural }
  | "REAL" -> { s with cfloat = Cfloat.Real }
  | "FLOAT" | "CFLOAT" -> { s with cfloat = Cfloat.Float }
  | t -> warning (Printf.sprintf
                  "In model spec %S : unknown '%s' selector@." m t) ; s

let apply_config ~warning (s:setup) m : setup =
  List.fold_left (update_config ~warning m) s (split ~warning m)

let default =
  {
    mheap = Typed MemTyped.Fits ;
    mvar = Var ;
    cint = Cint.Machine ;
    cfloat = Cfloat.Float ;
  }

let abort msg = Wp_parameters.abort "%s" msg

let parse ?(default=default) ?(warning=abort) opts =
  List.fold_left (apply_config ~warning) default opts
