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
(* --- Prover Why3 Interface                                          --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Qed
open Lang
open Definitions

let dkey = Wp_parameters.register_category "prover"
let why3_goal_name = "WP"

let option_file = LogicBuiltins.create_option
    (fun ~driver_dir x -> Filename.concat driver_dir x)
    "why3" "file"

let option_import = LogicBuiltins.create_option
    (fun ~driver_dir:_ x -> x)
    "why3" "import"


(* -------------------------------------------------------------------------- *)
(* --- Making Goal File                                                   --- *)
(* -------------------------------------------------------------------------- *)

let cluster_file c =
  let dir = Model.directory () in
  let base = cluster_id c in
  Printf.sprintf "%s/%s.why" dir base

let theory_name_of_cluster c =
  let base = cluster_id c in
  String.capitalize base

let theory_name_of_pid fmt pid =
  Format.fprintf fmt "VC%s" (WpPropId.get_propid pid)


(* -------------------------------------------------------------------------- *)
(* --- Exporting Formulae to Why3                                         --- *)
(* -------------------------------------------------------------------------- *)

type depend =
  | D_file of string
  | D_cluster of cluster

module TYPES = Model.Index
    (struct
      type key = adt
      type data = tau
      let name = "ProverWhy3.TYPES"
      let compare = ADT.compare
      let pretty = ADT.pretty
    end)

let engine =
  let module E = Qed.Export_why3.Make(Lang.F) in
  object
    inherit E.engine
    inherit Lang.idprinting
    method infoprover p = p.why3

    method set_typedef = TYPES.define
    method get_typedef = TYPES.get
    method typeof_call = Lang.tau_of_lfun
    method typeof_getfield = Lang.tau_of_field
    method typeof_setfield = Lang.tau_of_record
  end

let filenoext file =
  let basename = Filename.basename file in
  (try Filename.chop_extension basename
   with Invalid_argument _ -> basename)

let regexp_col = Str.regexp_string ":"
let regexp_com = Str.regexp_string ","

class visitor fmt c =
  object(self)

    inherit Definitions.visitor c
    inherit ProverTask.printer fmt (cluster_title c)

    val mutable deps = []

    (* --- Managing Formatter --- *)

    method flush =
      begin
        Format.pp_print_newline fmt () ;
        List.rev deps
      end

    (* --- Files, Theories and Clusters --- *)

    method add_dfile f =
      let df = D_file f in
      if not (List.mem df deps) then deps <- df :: deps

    method add_import ?was thy =
      self#lines ;
      match was with
      | None     -> Format.fprintf fmt "use import %s@\n" thy
      | Some was -> Format.fprintf fmt "use import %s as %s@\n" thy was

    method add_import2 file thy =
      self#lines ;
      Format.fprintf fmt "use import %s.%s@\n" file thy

    method add_import3 file thy name =
      self#lines ;
      Format.fprintf fmt "use import %s.%s as %s@\n" file thy name

    method on_cluster c =
      self#lines ;
      let name = (cluster_id c) in
      Format.fprintf fmt "use import %s.%s@\n"
        name (String.capitalize name) ;
      deps <- (D_cluster c) :: deps

    method add_extlib file =
      let thy = filenoext file in
      let path = LogicBuiltins.find_lib file in
      self#add_import2 thy (String.capitalize thy) ;
      self#add_dfile path

    method on_library thy =
      let iter_file opt =
        match Str.split_delim regexp_col opt with
        | [file] ->
            let filenoext = filenoext file in
            self#add_import2 filenoext (String.capitalize filenoext) ;
            self#add_dfile file
        | [file;lib] ->
            self#add_import2 (filenoext file) lib ;
            self#add_dfile file
        | [file;lib;name] ->
            self#add_import3 (filenoext file) lib name;
            self#add_dfile file
        | _ -> Wp_parameters.failure ~current:false
                 "Driver: why3.file %S not recognized (theory %s)"
                 opt thy
      in
      let iter_import opt =
        List.iter (fun import ->
            match Str.split_delim regexp_col import with
            | [ th ] -> self#add_import th
            | [ th ; was ] -> self#add_import ~was th
            | _ -> Wp_parameters.failure ~current:false
                     "Driver: why3.import %S not recognized (theory %s)"
                     opt thy
          ) (Str.split regexp_com opt)
      in
      begin
        List.iter iter_file
          (LogicBuiltins.get_option option_file ~library:thy) ;
        List.iter iter_import
          (LogicBuiltins.get_option option_import ~library:thy) ;
      end

    method on_type lt def =
      begin
        self#lines ;
        engine#declare_type fmt (Lang.atype lt) (List.length lt.lt_params) def ;
      end

    method on_comp c fts =
      begin
        (*TODO:NUPW: manage UNIONS *)
        self#lines ;
        engine#declare_type fmt (Lang.comp c) 0 (Qed.Engine.Trec fts) ;
      end

    method on_dlemma l =
      begin
        self#paragraph ;
        let kind = if l.l_assumed then "axiom" else "lemma" in
        engine#declare_prop ~kind fmt
          (Lang.lemma_id l.l_name)
          l.l_forall l.l_triggers
          (F.e_prop l.l_lemma)
      end

    method on_dfun d =
      begin
        self#paragraph ;
        match d.d_definition with
        | Logic t ->
            engine#declare_signature fmt
              d.d_lfun (List.map F.tau_of_var d.d_params) t ;
        | Value(t,mu,v) ->
            let pp = match mu with
              | Rec -> engine#declare_fixpoint ~prefix:"fix_"
              | Def -> engine#declare_definition
            in pp fmt d.d_lfun d.d_params t v
        | Predicate(mu,p) ->
            let pp = match mu with
              | Rec -> engine#declare_fixpoint ~prefix:"fix_"
              | Def -> engine#declare_definition
            in pp fmt d.d_lfun d.d_params Logic.Prop (F.e_prop p)
        | Inductive _ ->
            engine#declare_signature fmt
              d.d_lfun (List.map F.tau_of_var d.d_params) Logic.Prop
      end

  end

let write_cluster c =
  let f = cluster_file c in
  Wp_parameters.debug ~dkey "Generate '%s'" f ;
  Command.print_file f
    begin fun fmt ->
      let v = new visitor fmt c in
      let name = theory_name_of_cluster c in
      v#printf "@[<hv 2>theory %s@\n" name;
      v#lines ;
      (** TODO add them only when needed *)
      v#add_import "bool.Bool" ;
      v#add_import "int.Int" ;
      v#add_import "int.ComputerDivision" ;
      v#add_import "real.RealInfix" ;
      v#on_library "qed";
      v#add_import "map.Map" ;
      v#vself ;
      v#printf "@]@\nend@\n";
      v#flush ;
    end

(* -------------------------------------------------------------------------- *)
(* --- File Assembly                                                      --- *)
(* -------------------------------------------------------------------------- *)

module CLUSTERS = Model.Index
    (struct
      type key = cluster
      type data = int * depend list
      let name = "ProverWhy3.CLUSTERS"
      let compare = cluster_compare
      let pretty = pp_cluster
    end)

let assemble_cluster e =
  let rec assemble = function
    | D_cluster c -> assemble_cluster c
    | D_file path -> assemble_userlib path

  and assemble_cluster c =
    let (age,deps) = try CLUSTERS.find c with Not_found -> (-1,[]) in
    let deps =
      if age < cluster_age c then
        let deps = write_cluster c in
        CLUSTERS.update c (cluster_age c , deps) ; deps
      else deps in
    List.iter assemble deps

  and assemble_userlib source =
    if Filepath.normalize (Filename.dirname source) <>
       Filepath.normalize (Filename.concat (Wp_parameters.Share.dir ()) "why3")
    then
      let tgtdir = Model.directory () in
      let coqsrc = Filename.basename source in
      let target = Printf.sprintf "%s/%s" tgtdir coqsrc in
      Command.copy source target
  in
  assemble e

(* -------------------------------------------------------------------------- *)
(* --- Assembling Goal                                                    --- *)
(* -------------------------------------------------------------------------- *)
open Cil_datatype

let assemble_goal ~title ~id ~pid ~axioms prop fmt =
  (** Also create the directory *)
  let goal = cluster ~id ~title () in
  let deps =
    let v = new visitor fmt goal in
    v#printf "@[<hv 2>theory %a@\n" theory_name_of_pid pid;
    v#add_import "bool.Bool" ;
    v#add_import "int.Int" ;
    v#add_import "int.ComputerDivision" ;
    v#add_import "real.RealInfix" ;
    v#on_library "qed";
    v#add_import "map.Map" ;
    v#vgoal axioms prop ;
    let libs = Wp_parameters.WhyLibs.get () in
    if libs <> [] then
      begin
        v#section "Additional Libraries" ;
        List.iter v#add_extlib libs ;
        v#hline ;
      end ;
    v#paragraph ;
    engine#global
      begin fun () ->
        v#printf "@[<hv 2>goal %s \"expl:%s\":@ %a@]@\n@\n"
          why3_goal_name
          title
          engine#pp_prop (F.e_prop prop) ;
      end ;
    v#printf "end@]@.";
    v#flush
  in
  List.iter assemble_cluster deps

module FunFile = Model.Index
    (struct
      type key = kernel_function
      type data = int (* age *)
      let name = "ProverWhy3.FunFile"
      let compare = Kernel_function.compare
      let pretty = Kernel_function.pretty
    end)

type goal_id =
  {
    gfile : string;
    gtheory : string;
    ggoal : string;
  }

let assemble_wpo wpo =
  let dir = Model.directory () in
  let index = Wpo.get_index wpo in
  let file = match index with
    | Wpo.Axiomatic _ ->
        begin match wpo.Wpo.po_formula with
          | Wpo.GoalAnnot _ | Wpo.GoalCheck _ -> assert false
          | Wpo.GoalLemma vca ->
              let lemma = vca.Wpo.VC_Lemma.lemma in
              assemble_cluster (D_cluster lemma.l_cluster);
              let file = cluster_file lemma.l_cluster in
              {gfile = file;
               gtheory = theory_name_of_cluster lemma.l_cluster;
               ggoal = (Lang.lemma_id lemma.l_name);
              }
        end
    | Wpo.Function (kf,_behv) ->
        let model = Model.get_model () in
        let file = Wpo.DISK.file_kf ~kf ~model ~prover:VCS.Why3ide in
        let age = try FunFile.find kf with Not_found -> -1 in
        begin if age < Wpo.age wpo then
            let age_max = ref (-1) in
            let on_goal fmt wpo =
              (** iter on all the goal of a kf unfortunately not just
                  the one of the current model *)
              let pid = wpo.Wpo.po_pid in
              let model = Model.get_model () in
              if Model.S.equal wpo.Wpo.po_model model then begin
                age_max := max (!age_max) (Wpo.age wpo);
                match wpo.Wpo.po_formula with
                | Wpo.GoalAnnot vcq ->
                  let prop =  Wpo.GOAL.compute_proof vcq.Wpo.VC_Annot.goal in
                  if Lang.F.p_true != prop then
                    let id = WpPropId.get_propid pid in
                    let title = Pretty_utils.to_string WpPropId.pretty pid in
                    let axioms = None in
                    assemble_goal ~pid ~id ~title ~axioms prop fmt
                | Wpo.GoalLemma _ | Wpo.GoalCheck _ -> assert false
              end in
            Command.print_file file
              (fun fmt ->
                 let fun_index = Wpo.Function(kf,None) in
                 Wpo.iter ~index:fun_index ~on_goal:(on_goal fmt) ());
            assert (!age_max >= Wpo.age wpo);
            FunFile.update kf (!age_max);
        end;
        let pid = wpo.Wpo.po_pid in
        {
          gfile = file;
          gtheory = Pretty_utils.to_string theory_name_of_pid pid;
          ggoal = why3_goal_name;
        }
  in
  [dir], file

let assemble_check wpo vck =
  let open Wpo.VC_Check in
  let pid = wpo.Wpo.po_pid in
  let id = Printf.sprintf "Qed-%d-%d" (Lang.F.id vck.qed) (Lang.F.id vck.raw) in
  let goal = cluster ~id () in
  let file = cluster_file goal in
  Command.print_file file 
    (assemble_goal ~title:"Qed Check" ~id ~pid ~axioms:None vck.goal) ;
  let dir = Model.directory () in
  [dir], { 
    gfile = file ;
    gtheory = Pretty_utils.to_string theory_name_of_pid pid ;
    ggoal = why3_goal_name ;
  }

let assemble_wpo wpo =
  match wpo.Wpo.po_formula with
  | Wpo.GoalCheck vck ->
    Some (Model.with_model wpo.Wpo.po_model (assemble_check wpo) vck)
  | Wpo.GoalAnnot vcq
    when Lang.F.p_true == Wpo.GOAL.compute_proof vcq.Wpo.VC_Annot.goal ->
    (** The wpo is trivial *)
      None
  | _ ->
      Some (Model.with_model wpo.Wpo.po_model assemble_wpo wpo)

(* -------------------------------------------------------------------------- *)
(* --- Running Why3                                                       --- *)
(* -------------------------------------------------------------------------- *)

open ProverTask

let p_goal = p_until_space ^ " " ^ p_until_space ^ " " ^ p_until_space ^ " : "
let p_valid = p_goal ^ "Valid (" ^ p_float ^ "s)"
let p_unknown = p_goal ^ "Unknown (" ^ p_float ^ "s)"
let p_limit = p_goal ^ "Timeout"
let p_error = "File " ^ p_string ^ ", line " ^ p_int ^ ", characters "
              ^ p_int ^ "-" ^ p_int ^ ":\n\\(warning:\\)?"

let re_valid = Str.regexp p_valid
let re_unknown = Str.regexp p_unknown
let re_limit = Str.regexp p_limit
let re_error = Str.regexp p_error

type error =
  | Error_No
  | Error_Prover of string
  | Error_Generated of Lexing.position * string

let rec split spec i =
  try
    let j = String.index_from spec i ':' in
    if j > i then
      String.sub spec i (j-i) :: split spec (succ j)
    else
      split spec (succ j)
  with Not_found ->
    let n = String.length spec - i in
    if n > 0 then [ String.sub spec i n ] else []

let chop_version spec = match split spec 0 with
  | [] | [_] -> spec
  | [a;b] -> Printf.sprintf "%s,%s," a b
  | a::b::c::_ -> Printf.sprintf "%s,%s,%s" a b c

class why3 ~prover ~pid ~file ~includes ~logout ~logerr =
  object(why)

    initializer ignore pid

    inherit ProverTask.command "why3"

    val mutable files = []
    val mutable error = Error_No
    val mutable valid = false
    val mutable limit = false
    val mutable time = 0.0

    method private time t = time <- t

    method private error (a : pattern) =
      try
        let _warning = a#get_string 5 in
        ()
      with Not_found ->
        let lpos = ProverTask.location (a#get_string 1) (a#get_int 2) in
        error <- Error_Generated ( lpos , a#get_after ~offset:1 4 )

    method private unknown_prover (a : pattern) =
      error <- Error_Prover (a#get_string 0)

    method private valid (a : pattern) =
      begin
        valid <- true ;
        time <- a#get_float 4 ;
      end

    method private limit (_a : pattern) =
      begin
        limit <- true ;
      end

    method result r =
      match error with
      | Error_Prover message ->
          Wp_parameters.error "Why3:@\n%s" message;
          VCS.failed message
      | Error_Generated(pos,message) ->
          Wp_parameters.error ~source:pos "Why3 error:@\n%s" message ;
          VCS.failed ~pos message
      | Error_No ->
          if r = 0 then
            let verdict =
              if valid then VCS.Valid else
              if limit then VCS.Timeout else
                VCS.Unknown in
            VCS.result ~time verdict
          else
            begin
              ProverTask.pp_file ~message:"Why3 (stdout)" ~file:logout ;
              ProverTask.pp_file ~message:"Why3 (stderr)" ~file:logerr ;
              VCS.failed (Printf.sprintf "Why3 exits with status [%d]" r)
            end

    method prove =
      let time = Wp_parameters.Timeout.get () in
      if Wp_parameters.Check.get () then why#add ["--type-only"] ;
      why#add ["--extra-config"; Wp_parameters.Share.file "why3/why3.conf"];
      why#add (Wp_parameters.WhyFlags.get ()) ;
      why#add [ file.gfile ] ;
      why#add ["-P";chop_version prover];
      why#add ["-T";file.gtheory];
      why#add ["-G";file.ggoal];
      why#add_positive ~name:"-t" ~value:time ;
      if Wp_parameters.ProofTrace.get () then
        (* [VP] This also keeps temp files. To be changed with FB's new option
           	 when it is implemented. *)
        why#add ["--debug"; "call_prover"];
      why#timeout time ;
      why#add_list ~name:"-I" includes;
      why#add ["-I";Wp_parameters.Share.file "why3"];
      why#validate_time why#time ;
      (* The order is important. Warning are detected as error 
         which they are not. *)
      why#validate_pattern ~logs:`OUT re_limit why#limit ;
      why#validate_pattern ~logs:`ERR re_error why#error ;
      why#validate_pattern ~logs:`OUT re_valid why#valid ;
      why#run ~logout ~logerr

  end

open VCS
open Wpo
open Task

let prove_file ~prover ~pid ~file ~includes ~logout ~logerr =
  let why = new why3 ~prover ~pid ~file ~includes ~logout ~logerr in
  why#prove () >>> function
  | Task.Timeout -> Task.return VCS.timeout
  | Task.Result r -> Task.call why#result r
  | st -> Task.status (Task.map (fun _ -> assert false) st)

let prove_prop ~prover ~wpo =
  match assemble_wpo wpo with
  | None -> Task.return VCS.no_result
  | Some (includes,file) ->
      if Wp_parameters.Generate.get () 
      then Task.return VCS.no_result
      else
        let model = wpo.po_model in
        let pid = wpo.Wpo.po_pid in
        let logout = DISK.file_logout ~pid ~model ~prover:(Why3 prover) in
        let logerr = DISK.file_logerr ~pid ~model ~prover:(Why3 prover) in
        prove_file ~prover ~pid ~file ~includes ~logout ~logerr

let prove wpo ~prover =
  Task.todo (fun () -> prove_prop ~wpo ~prover)

(* -------------------------------------------------------------------------- *)
(* --- Why3-Ide                                                           --- *)
(* -------------------------------------------------------------------------- *)

class why3ide ~includes ~files ~session =
  object(why)

    inherit ProverTask.command "why3ide"

    method start () =
      why#add ["--extra-config"; Wp_parameters.Share.file "why3/why3.conf"];
      why#add (Wp_parameters.WhyFlags.get ()) ;
      why#add_list ~name:"-I" includes;
      why#add ["-I";Wp_parameters.Share.file "why3"];
      why#add [session];
      why#add files;
      why#run ~echo:true ()

  end

let call_ide ~includes ~files ~session =
  if Wp_parameters.Generate.get ()
  then Task.return false
  else
    let why = new why3ide ~includes ~files ~session in
    Task.todo why#start >>= fun s -> Task.return (s=0)

(* -------------------------------------------------------------------------- *)
(* --- Why3-Config                                                        --- *)
(* -------------------------------------------------------------------------- *)

type dp = {
  dp_name : string ;
  dp_version : string ;
  dp_prover : string ;
}

let find name dps =
  try List.find (fun d -> d.dp_prover = name) dps
  with Not_found ->
    let name = String.lowercase name in
    try List.find (fun d -> String.lowercase d.dp_name = name) dps
    with Not_found -> 
      { dp_prover = name ; dp_name = name ; dp_version = "default" }

let parse spec =
  try
    let k = String.index spec ':' in
    let dp_name = String.sub spec 0 k in
    let dp_version = String.sub spec (succ k) (String.length spec - k - 1) in
    for i = 0 to String.length dp_version - 1 do
      if dp_version.[i] = ':' then dp_version.[i] <- ' ' ;
    done ;
    { dp_prover = spec ; dp_name ; dp_version }
  with Not_found ->
    { dp_prover = spec ; dp_name = spec ; dp_version = "default" }

let pe_prover = Str.regexp "\\([^ ]+\\) (\\([^)]+\\))"

class why3detect job =
  object(why)

    inherit ProverTask.command "why3"

    val mutable dps = []

    method result st = 
      job (if st = 0 then Some (List.rev dps) else None)

    method prover p =
      begin
        let dp_name = p#get_string 1 in
        let dp_version = p#get_string 2 in
        Wp_parameters.debug ~level:1 
          "Prover %S, version %s detected." dp_name dp_version ;
        let dp_prover = Printf.sprintf "%s:%s" dp_name dp_version in
        for i = String.length dp_name + 1 to String.length dp_prover - 1 do
          match dp_prover.[i] with
          | ' ' | ',' -> dp_prover.[i] <- ':'
          | _ -> ()
        done ;
        dps <- { dp_name ; dp_version ; dp_prover } :: dps
      end

    method detect : unit task =
      begin
        why#add [ "--list-provers" ] ;
        why#validate_pattern ~repeat:true ~logs:`OUT pe_prover why#prover ;
        why#run ~echo:true () >>= Task.call why#result
      end

  end

let detect_why3 job = Task.run ((new why3detect job)#detect)
let detect_provers job = 
  detect_why3 (function None -> job [] | Some dps -> job dps)
