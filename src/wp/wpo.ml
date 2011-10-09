(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* ------------------------------------------------------------------------ *)
(* ---  Proof Obligations                                               --- *)
(* ------------------------------------------------------------------------ *)

open Cil_types

type po = t and t =
{
  po_fun   : kernel_function ; (* function *)
  po_bhv   : string option ;   (* behavior *)
  po_pid   : WpPropId.prop_id ; (* goal target property *)
  po_gid   : string ;  (* goal identifier *)
  po_env   : string ;  (* goal environment identifier *)
  po_model : string ;  (* model identifier *)
  po_updater : Emitter.t ; (* property status updater *)
  po_name  : string ;  (* goal informal name *)
  po_dep   : Property.t list ; (* dependencies *)
  po_warn  : warning list ;    (* warnings *)
}

and warning = {
  wrn_loc : Lexing.position ;
  wrn_severe : bool ;
  wrn_source : string ;
  wrn_reason : string ;
  wrn_effect : string ;
}

module PODatatype =
  Datatype.Make(
    struct
      type t = po
      include Datatype.Undefined
      let name = "Wpo.po"
      let reprs =
        [ { po_fun = List.hd Kernel_function.reprs;
            po_bhv = Some "Cil.default_behavior_name";
            po_pid = List.hd WpPropId.Prop_id_datatype.reprs;
            po_gid = "xxx";
            po_env = "";
            po_model = "Store";
            po_updater = List.hd Emitter.reprs;
            po_name = "dummy";
            po_dep = [];
            po_warn = [];}]
    end)

type prover =
  | Why of string (* Prover via WHY *)
  | AltErgo       (* Alt-Ergo *)
  | Coq           (* Coq and Coqide *)
  | WP            (* Simplifier *)

module ProverType =
  Datatype.Make
    (struct
      type t = prover
      include Datatype.Undefined
      let name = "Wpo.prover"
      let reprs = [ AltErgo; Coq; WP; Why "z3" ]
     end)

type language =
  | L_why
  | L_coq
  | L_altergo

type result =
  | Valid
  | Invalid
  | Unknown
  | Timeout
  | Computing
  | Failed of string

module ResultType =
  Datatype.Make
    (struct
        type t = result
        include Datatype.Undefined
        let name = "Wpo.result"
        let reprs = [ Valid; Invalid; Unknown; Timeout;
                      Computing; Failed "error" ]
     end)

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printers                                                    --- *)
(* -------------------------------------------------------------------------- *)

let pp_warning fmt w =
  begin
    Format.fprintf fmt
      "@[<v 0>%s:%d: warning from %s:@\n"
      w.wrn_loc.Lexing.pos_fname
      w.wrn_loc.Lexing.pos_lnum
      w.wrn_source ;
    if w.wrn_severe then
      Format.fprintf fmt " - Warning: %s, looking for context inconsistency"
        w.wrn_effect
    else
      Format.fprintf fmt " - Warning: %s" w.wrn_effect ;
    Format.fprintf fmt "@\n   Reason: %s@]" w.wrn_reason ;
  end

let pp_dependency kf fmt d =
  Format.fprintf fmt " - Assumes %a" (Description.pp_localized ~kf:(`Context kf) ~ki:false) d

let pp_depend fmt d =
  Format.fprintf fmt " - Assumes %a" (Description.pp_localized ~kf:`Always ~ki:false) d

let pp_prover fmt = function
  | AltErgo -> Format.pp_print_string fmt "Alt-Ergo"
  | Coq -> Format.pp_print_string fmt "Coq"
  | Why smt ->
      if Wp_parameters.debug_atleast 1 then
         Format.pp_print_string fmt ("Why:"^(String.capitalize smt))
      else
        Format.pp_print_string fmt (String.capitalize smt)
  | WP -> Format.fprintf fmt "WP"

let pp_language fmt = function
  | L_altergo -> Format.pp_print_string fmt "Alt-Ergo"
  | L_coq -> Format.pp_print_string fmt "Coq"
  | L_why -> Format.pp_print_string fmt "Why"

let pp_result fmt = function
  | Valid -> Format.pp_print_string fmt "Valid"
  | Invalid -> Format.pp_print_string fmt "Invalid"
  | Unknown -> Format.pp_print_string fmt "Unknown"
  | Timeout -> Format.pp_print_string fmt "Timeout"
  | Computing -> Format.pp_print_string fmt "Computing"
  | Failed msg ->
      if Wp_parameters.debug_atleast 1
      then Format.fprintf fmt "Failed@\nError: %s" msg
      else Format.fprintf fmt "Failed"

(* -------------------------------------------------------------------------- *)
(* --- Proof Collector                                                    --- *)
(* -------------------------------------------------------------------------- *)

module Hproof = Hashtbl.Make(Datatype.Pair(Datatype.String)(Property))
  (* Table indexed by ( Model name , Property proved ) *)

module Results =
struct
  type t = (prover,result) Hashtbl.t
  let create () = Hashtbl.create 7
  let replace t p r = Hashtbl.replace t p r
  let clear t = Hashtbl.clear t
  let remove t p = Hashtbl.remove t p
  let get t p = try Some (Hashtbl.find t p) with Not_found -> None
  let iter = Hashtbl.iter
end

(* -------------------------------------------------------------------------- *)
(* --- Wpo Database                                                       --- *)
(* -------------------------------------------------------------------------- *)

module WPO =
struct
  type t = po
  let hash t = Hashtbl.hash t.po_gid
  let equal a b = (a.po_gid = b.po_gid)
    let compare a b =
      let c = String.compare a.po_model b.po_model in
      if c<>0 then c else
        let c = String.compare a.po_name b.po_name in
        if c<>0 then c else
          let c = WpPropId.compare_prop_id a.po_pid b.po_pid in
          if c<>0 then c else String.compare a.po_gid b.po_gid
end

module Kfmap = Kernel_function.Map
module Imap = Map.Make(String)
module WPOset = Set.Make(WPO)
module Hpo = Hashtbl.Make(WPO)

type system = {
  mutable environments : int Imap.t ;
  (* context name -> # of environment *)
  mutable last : (string * int * string) option ;
  (* last environment generated *)
  mutable index : WPOset.t Imap.t Kfmap.t ;
  (* all Wpo added, indexed by kernel-function and behavior *)
  proofs : WpAnnot.proof Hproof.t ;
  (* proof collector *)
  results : Results.t Hpo.t ;
  (* results collector *)
}

let system = {
  last = None ;
  environments = Imap.empty ;
  index = Kfmap.empty ;
  proofs = Hproof.create 131 ;
  results = Hpo.create 131 ;
}

(* -------------------------------------------------------------------------- *)
(* --- Getters                                                            --- *)
(* -------------------------------------------------------------------------- *)

let get_gid =
  Dynamic.register
    ~plugin:"Wp" "Wpo.get_gid" ~journalize:false
    (Datatype.func PODatatype.ty Datatype.string)
    (fun g -> g.po_gid)

let get_prop_id =
  Dynamic.register
    ~plugin:"Wp" "Wpo.get_prop_id" ~journalize:false
    (Datatype.func PODatatype.ty WpPropId.Prop_id_datatype.ty)
    (fun g -> g.po_pid)

(* ------------------------------------------------------------------------ *)
(* ---  WPO Construction                                                --- *)
(* ------------------------------------------------------------------------ *)

(* A WPO is uniquely determined by :
   1. The context name (unique per updater by construction)
   2. The kernel-function
   3. The behavior
   4. The target prop-id
*)

let gid ~context ~kf ~bhv ~propid =
  let gname = WpPropId.prop_id_name propid in
  let fname = Kernel_function.get_name kf in
  match bhv with
    | Some b -> Printf.sprintf "%s_%s_%s_%s" context fname b gname
    | None -> Printf.sprintf "%s_%s_%s" context fname gname

(* -------------------------------------------------------------------------- *)
(* --- Registry of POs                                                    --- *)
(* -------------------------------------------------------------------------- *)

let clear () =
  begin
    system.index <- Kfmap.empty ;
    Hproof.clear system.proofs ;
    Hpo.clear system.results ;
  end

let env_name model k = Printf.sprintf "%s_env%d" model k

let new_env ~context =
  let k =
    try succ (Imap.find context system.environments)
    with Not_found -> 1 in
  system.environments <- Imap.add context k system.environments ;
  let env = env_name context k in
  system.last <- Some (context,k,env) ; env

let release_env ~env =
  match system.last with
    | Some (model,k0,env0) ->
        if env0 = env then
          system.environments <-
            Imap.add model (pred k0) system.environments
    | None -> ()

let add g =
  begin
    let bmap =
      try Kfmap.find g.po_fun system.index
      with Not_found -> Imap.empty
    in
    let bhv = match g.po_bhv with None -> "" | Some b -> b in
    let pset =
      try Imap.find bhv bmap
      with Not_found -> WPOset.empty
    in
    if WPOset.mem g pset then
      begin
        let pi = ( g.po_model , WpPropId.property_of_id g.po_pid ) in
        Hproof.remove system.proofs pi ;
        Hpo.remove system.results g ;
      end ;
    let pset' = WPOset.add g pset in
    let bmap' = Imap.add bhv pset' bmap in
    system.index <- Kfmap.add g.po_fun bmap' system.index ;
  end

let set_po_result g r =
  try
    let pi = ( g.po_model , WpPropId.property_of_id g.po_pid ) in
    let proof =
      try Hproof.find system.proofs pi
      with Not_found ->
        let proof = WpAnnot.create_proof g.po_pid in
        Hproof.add system.proofs pi proof ; proof
    in
    if r = Valid then WpAnnot.add_proof proof g.po_pid g.po_dep ;
    let status = 
      if WpAnnot.is_proved proof then Property_status.True 
      else Property_status.Dont_know
    in
    let target = WpAnnot.target proof in
    let depends = WpAnnot.dependencies proof in
    Property_status.emit g.po_updater ~hyps:depends target status ;
  with
    (* [JS 2011/01/28] Please do not catch Log.* exception. Let the kernel do
       the job *)
    | Log.AbortFatal plugin as err ->
        Wp_parameters.failure
          "Update-status failed (problem in %s)" plugin ;
        raise err
    | err ->
        Wp_parameters.failure
          "Update-status failed (%s)" (Printexc.to_string err);
        raise err

let set_result g p r =
  begin
    let rs =
      try Hpo.find system.results g
      with Not_found ->
        let rs = Results.create () in
        Hpo.add system.results g rs ; rs
    in
    Results.replace rs p r ;
    set_po_result g r ;
    if p = WP then Wp_parameters.result "[WP:simplified] Goal %s : Valid" g.po_gid ;
  end

let get_result g p =
  try Results.get (Hpo.find system.results g) p
  with Not_found -> None

let get_result =
  Dynamic.register ~plugin:"Wp" "Wpo.get_result" ~journalize:false
    (Datatype.func2 PODatatype.ty ProverType.ty
       (Datatype.option ResultType.ty))
    get_result

let is_valid =
  Dynamic.register ~plugin:"Wp" "Wpo.is_valid" ~journalize:false
    (Datatype.func ResultType.ty Datatype.bool)
    (function Valid -> true | _ -> false)



let get_results g =
  try
    let a = ref [] in
    Results.iter (fun p r -> a:=(p,r)::!a) (Hpo.find system.results g) ; !a
  with Not_found -> []

(* -------------------------------------------------------------------------- *)
(* --- Iterator                                                           --- *)
(* -------------------------------------------------------------------------- *)

let iter ?on_environment ?on_function ?on_behavior ?on_goal () =
  begin
    match on_environment with
    | None -> ()
    | Some phi ->
      Imap.iter
        (fun m k ->
          for i = 1 to k do
            phi (env_name m i)
          done)
        system.environments
  end ;
  if on_function <> None || on_behavior <> None || on_goal <> None
  then
    let sorted_index =
      List.sort
        (fun (k1,_) (k2,_) ->
          if Kernel_function.equal k1 k2 then 0 else
            if Kernel_function.get_name k1 <= Kernel_function.get_name k2
            then -1 else 1)
        (Kfmap.fold (fun k v a -> (k,v)::a) system.index [])
    in
    List.iter
      (fun (kf,bmap) ->
        if (on_behavior <> None || on_goal <> None) &&
          not (Imap.is_empty bmap)
        then
          begin
            ( match on_function with
            | None -> ()
            | Some phi -> phi kf ) ;
            let sorted_behaviors =
              List.sort
                (fun (k1,_) (k2,_) -> String.compare k1 k2)
                (Imap.fold (fun k v a -> (k,v)::a) bmap [])
            in
            List.iter
              (fun (b,pset) ->
                if not (WPOset.is_empty pset) then
                  begin
                    ( match on_behavior with
                    | None -> ()
                    | Some phi ->
                      phi kf (if b="" then None else Some b) ) ;
                    match on_goal with
                    | None -> ()
                    | Some f -> WPOset.iter f pset
                  end)
              sorted_behaviors
          end)
      sorted_index

let iter_on_goals =
  Dynamic.register ~plugin:"Wp" "Wpo.iter_on_goals"
    (Datatype.func (Datatype.func PODatatype.ty Datatype.unit) Datatype.unit)
    ~journalize:true
    (fun on_goal -> iter ~on_goal ())

(* -------------------------------------------------------------------------- *)
(* --- Prover and Files                                                   --- *)
(* -------------------------------------------------------------------------- *)

let local base suffix =
  let dir = Wp_parameters.get_output () in
  Printf.sprintf "%s/%s%s" dir base suffix

let file_for_lang base lang =
  let dir = Wp_parameters.get_output () in
  let suffix =
    match lang with
      | L_altergo -> "_ergo.why"
      | L_why -> ".why"
      | L_coq -> ".v"
  in Printf.sprintf "%s/%s%s" dir base suffix

let file_for_ctxt ~env = local env ".txt"
let file_for_head ~gid = local gid "_head.txt"
let file_for_body ~gid = local gid "_body.txt"
let file_for_log_proof ~gid = function
  | Why s -> local gid ("_" ^s^".txt")
  | Coq -> local gid "_coq.txt"
  | AltErgo -> local gid "_ergo.txt"
  | WP -> local gid "_wp.txt"

let file_for_log_proof_ =
  Dynamic.register ~plugin:"Wp" "Wpo.file_for_log_proof" ~journalize:false
    (Datatype.func2
       ~label1:("gid",None) Datatype.string ProverType.ty Datatype.string)
    (fun gid p -> file_for_log_proof ~gid p)

let file_for_log_check ~gid = function
  | L_why -> local gid "_why.txt"
  | L_coq -> local gid "_coq.txt"
  | L_altergo -> local gid "_ergo.txt"

let file_for_goal ~gid lang = file_for_lang gid lang
let file_for_env ~env lang = file_for_lang env lang
let file_for_po ~gid lang = file_for_lang (gid ^ "_po") lang

let file_for_model ~model lang =
  let dshare = Wp_parameters.get_share() in
  let suffix =
    match lang with
      | L_altergo -> "_ergo.why"
      | L_why -> "_model.why"
      | L_coq  -> "_model.v"
  in Printf.sprintf "%s/%s%s" dshare model suffix

let coq_for_env ~env = env
let coq_for_model ~model = model^"_model"
let coqc_for_model ~model =
  let dir = Wp_parameters.get_output () in
  Printf.sprintf "%s/%s_model.v" dir model

let prover_of_name = function
  | "" | "none" -> None
  | "alt-ergo" -> Some AltErgo
  | "coq" | "coqide" -> Some Coq
  | s -> Some (Why s)

let prover_of_name =
  Dynamic.register ~plugin:"Wp" "Wpo.prover_of_name" ~journalize:false
    (Datatype.func Datatype.string (Datatype.option ProverType.ty))
    prover_of_name

let language_of_name = function
  | "" | "none" -> None
  | "alt-ergo" -> Some L_altergo
  | "coq" | "coqide"-> Some L_coq
  | "why" -> Some L_why
  | s -> Wp_parameters.abort "Language '%s' unknown" s

let language_of_prover = function
  | Why _ -> L_why
  | Coq -> L_coq
  | AltErgo -> L_altergo
  | WP -> L_why


let language_of_prover_name = function
  | "" | "none" -> None
  | "alt-ergo" -> Some L_altergo
  | "coq" | "coqide" -> Some L_coq
  | _ -> Some L_why


let is_interactive = function
  | "coqide" -> true
  | _ -> false

let gui_provers = [ WP ; AltErgo ; Coq ;
                    Why "z3" ; Why "simplify" ;
                    Why "vampire";
                    Why "cvc3" ; Why "yices" ;
                    Why "zenon" ]


(* -------------------------------------------------------------------------- *)
(* --- Proof Obligations : Pretty-printing                                --- *)
(* -------------------------------------------------------------------------- *)

let bar = String.make 60 '-'
let flow = ref false

let pp_environment fmt env =
  Format.fprintf fmt
    "%s@\n  Proof Environment %s@\n%s@\n@\n%a"
    bar (String.capitalize env) bar
    Command.pp_from_file (file_for_ctxt ~env)

let pp_function fmt kf bhv =
  flow := true ;
  match bhv with
    | None ->
        Format.fprintf fmt
          "%s@\n  Function %s@\n%s@\n@\n"
          bar (Kernel_function.get_name kf) bar
    | Some bhv ->
        Format.fprintf fmt
          "%s@\n  Function %s with behavior %s@\n%s@\n@\n"
          bar (Kernel_function.get_name kf) bhv bar

let pp_goal fmt g =
  begin
    Format.fprintf fmt "@[<v 0>Proof Obligation %s:@]@\n" g.po_name ;
    List.iter
      (fun (prover,result) ->
         Format.fprintf fmt "Prover %a returns %a@\n"
           pp_prover prover
           pp_result result ;
      ) (get_results g) ;
    Format.fprintf fmt "Environment: %s@\n" (String.capitalize g.po_env) ;
    List.iter (fun d -> Format.fprintf fmt "%a@\n" (pp_dependency g.po_fun) d) g.po_dep ;
    List.iter (fun w -> Format.fprintf fmt "%a@\n" pp_warning w) g.po_warn ;
    Format.fprintf fmt " + Proves %a@\n" (WpPropId.pretty_context g.po_fun) g.po_pid ;
    Command.pp_from_file fmt (file_for_body ~gid:g.po_gid)
  end

let pp_goal_flow fmt g =
  begin
    if not !flow then Format.pp_print_newline fmt () ;
    pp_goal fmt g ;
    Format.fprintf fmt "@\n%s@." bar ;
    flow := false ;
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
