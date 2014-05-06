(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

type mheap = Hoare | Typed of MemTyped.pointer
type mvar = Raw | Var | Ref

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
    Buffer.add_string t (String.capitalize name) ;
  end

let add (i,t) part = 
  begin
    Buffer.add_char i '_' ;
    Buffer.add_string i part ;
    Buffer.add_char t ' ' ;
    Buffer.add_char t '(' ;
    Buffer.add_string t (String.capitalize part) ;
    Buffer.add_char t ')' ;
  end

let descr_mtyped d = function
  | MemTyped.NoCast -> add d "nocast"
  | MemTyped.Unsafe -> add d "cast"
  | MemTyped.Fits -> ()

let descr_mheap d = function
  | Hoare -> main d "hoare"
  | Typed p -> main d "typed" ; descr_mtyped d p

let descr_mvar d = function
  | Var -> ()
  | Ref -> add d "ref"
  | Raw -> add d "raw"

let descr_cint d = function
  | Cint.Natural -> ()
  | Cint.Machine -> add d "int"

let descr_cfloat d = function
  | Cfloat.Real -> ()
  | Cfloat.Float -> add d "float" 

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
let descr s = 
  try Hashtbl.find descriptions s
  with Not_found -> let w = descr_setup s in Hashtbl.add descriptions s w ; w

(* -------------------------------------------------------------------------- *)
(* --- Generator & Model                                                  --- *)
(* -------------------------------------------------------------------------- *)

module VarHoare : MemVar.VarUsage = 
struct 
  let datatype = "Value"
  let param _x = MemVar.ByValue 
end
module VarRef0 : MemVar.VarUsage = 
struct
  let datatype = "Ref0"
  let param x = match Variables_analysis.dispatch_cvar x with
    | Variables_analysis.Fvar -> MemVar.ByValue
    | _ -> MemVar.InHeap
end
module VarRef2 : MemVar.VarUsage = 
struct
  let datatype = "Ref2"
  let param x = match VarUsage.of_cvar x with
    | VarUsage.NotUsed | VarUsage.ByValue | VarUsage.ByArray _ 
    | VarUsage.ByRefArray _ -> MemVar.ByValue
    | VarUsage.ByReference -> MemVar.ByRef
    | VarUsage.ByAddress -> MemVar.InHeap
end

module MHoareVar = MemVar.Make(VarHoare)(MemEmpty)
module MHoareRef = MemVar.Make(VarRef2)(MemEmpty)
module MTypedVar = MemVar.Make(VarRef0)(MemTyped)
module MTypedRef = MemVar.Make(VarRef2)(MemTyped)

module WP_HoareVar = CfgWP.Computer(MHoareVar)
module WP_HoareRef = CfgWP.Computer(MHoareRef)
module WP_TypedRaw = CfgWP.Computer(MemTyped)
module WP_TypedVar = CfgWP.Computer(MTypedVar)
module WP_TypedRef = CfgWP.Computer(MTypedRef)

let wp (s:setup) : Model.t -> Generator.computer =
  match s.mheap , s.mvar with
    | Hoare , (Raw|Var) -> WP_HoareVar.create
    | Hoare ,   Ref     -> WP_HoareRef.create
    | Typed _ , Raw     -> WP_TypedRaw.create
    | Typed _ , Var     -> WP_TypedVar.create
    | Typed _ , Ref     -> WP_TypedRef.create

(* -------------------------------------------------------------------------- *)
(* --- Tuning                                                             --- *)
(* -------------------------------------------------------------------------- *)

let configure_mheap = function
  | Hoare -> MemEmpty.configure ()
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

type instance = {
  model : Model.t ;
  driver : LogicBuiltins.driver ;
}

let instances = ref MODEL.empty

let instance (s:setup) (d:driver) =
  try MODEL.find (s,d) !instances
  with Not_found ->
    let id,descr = descr s in
    let tuning = [configure s d] in
    let id,descr = 
      if LogicBuiltins.is_default d then id,descr
      else 
	( id ^ "_" ^ LogicBuiltins.id d ,
	  descr ^ " (Driver " ^ LogicBuiltins.descr d ^ ")" )
    in
    let model = Model.register ~id ~descr ~tuning () in
    let instance = { model = model ; driver = d } in
    instances := MODEL.add (s,d) instance !instances ; instance
      
let ident s = fst (descr s)
let descr s = snd (descr s)
let computer (s:setup) (d:driver) = wp s (instance s d).model

let split (m:string) : string list =
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
	 | _ -> Wp_parameters.error 
	     "In model spec %S : unexpected character '%c'" m c
    ) (String.uppercase m) ;
  flush () ; !tk

let rec update_config m s = function
  | "HOARE" -> { s with mheap = Hoare }
  | "TYPED" -> { s with mheap = Typed MemTyped.Fits }
  | "CAST" -> { s with mheap = Typed MemTyped.Unsafe }
  | "NOCAST" -> { s with mheap = Typed MemTyped.NoCast }
  | "RAW" -> { s with mvar = Raw }
  | "REF" -> { s with mvar = Ref }
  | "VAR" -> { s with mvar = Var }
  | "NAT" -> { s with cint = Cint.Natural }
  | "INT" | "CINT" -> { s with cint = Cint.Machine }
  | "REAL" -> { s with cfloat = Cfloat.Real }
  | "FLOAT" | "CFLOAT" -> { s with cfloat = Cfloat.Float }
  | t -> Wp_parameters.error 
      "In model spec %S : unknown '%s' selector@." m t ; s

let apply_config (s:setup) m : setup =
  List.fold_left (update_config m) s (split m)

let parse = List.fold_left apply_config {
  mheap = Typed MemTyped.Fits ;
  mvar = Var ;
  cint = Cint.Natural ;
  cfloat = Cfloat.Real ;
}
