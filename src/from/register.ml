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

(* $Id: register.ml,v 1.67 2008/04/18 11:04:02 uid568 Exp $ *)

open Cil_types
open Cil
open Db_types
open Db
open Locations
open Cilutil
open Abstract_interp
open Abstract_value

exception Call_did_not_take_place

module Functionwise_Dependencies =
  Kernel_function.Make_Table
    (Function_Froms.Datatype)
    (struct
       let name = Project.Computation.Name.make "functionwise_from"
       let size = 97
       let dependencies = [ Value.self ]
     end)

let () = 
  Db.From.self := Functionwise_Dependencies.self;
  Db.From.is_computed := Functionwise_Dependencies.mem

module Callwise_Dependencies =
  Kernel_computation.InstrHashtbl
    (Function_Froms.Datatype)
    (struct
       let name = Project.Computation.Name.make "callwise_from"
       let size = 97
       let dependencies = [ Value.self ]
     end)

module type Froms_To_Use_Sig = sig
  val get : kernel_function -> kinstr -> Function_Froms.t
end

module type Values_To_Use_Sig = sig
  val lval_to_loc_with_deps :
    (Cil_types.kinstr ->
      with_alarms:CilE.warn_mode ->
      skip_base_deps:bool ->
      deps:Locations.Zone.t ->
      Cil_types.lval -> Locations.Zone.t * Locations.location) ref
  val expr_to_kernel_function :
    (Cil_types.kinstr ->
      with_alarms:CilE.warn_mode ->
      deps:Locations.Zone.t option ->
      Cil_types.exp -> Locations.Zone.t * Db_types.kernel_function list) ref

  val get_state : Cil_types.kinstr -> Db.Value.state
  val access_expr : (Cil_types.kinstr -> Cil_types.exp -> Db.Value.t) ref
end

module type Recording_Sig = sig
  val record_kf : kernel_function -> Function_Froms.t -> unit
    (* function to call at the end of the treatment of a function *)
end

module Make
  (Values_To_Use:Values_To_Use_Sig)
  (Froms_To_Use: Froms_To_Use_Sig)
  (Recording_To_Do: Recording_Sig) =
struct
  type t' =
      { additional_deps_table : Zone.t StmtMap.t;
	(** Additional dependencies to add to all modified variables.
            Example: variables in the condition of an IF. *)
	additional_deps : Zone.t;
	(** Union of the sets in StmtMap.t *)
	deps_table : Lmap_bitwise.From_Model.t
	  (** dependency table *)
      }

  let call_stack : kernel_function Stack.t = Stack.create ()
  (** Stack of function being processed *)

  let rec find_deps_no_transitivity instr expr =
    (* The value of the expression [expr], just before executing the statement
       [instr], is a function of the values of the returned zones. *)
    match Cil.stripInfo expr with
    | Info _ -> assert false
    |AlignOfE _| AlignOf _| SizeOfStr _
    |SizeOfE _| SizeOf _ | Const _
        -> Zone.bottom
    | AddrOf lv  | StartOf lv ->
        let deps, _ = !Values_To_Use.lval_to_loc_with_deps
          ~with_alarms:CilE.warn_none_mode
          ~skip_base_deps:true
          ~deps:Zone.bottom
          instr
          lv
        in deps
    | CastE (_, e)|UnOp (_, e, _) ->
        find_deps_no_transitivity instr e
    | BinOp (_, e1, e2, _) ->
        Zone.join
          (find_deps_no_transitivity instr e1)
          (find_deps_no_transitivity instr e2)
    | Lval v ->
        find_deps_lval_no_transitivity instr v

  and find_deps_offset_no_transitivity instr o =
    match o with
    | NoOffset -> Zone.bottom
    | Field (_,o) -> find_deps_offset_no_transitivity instr o
    | Index (e,o) ->
        Zone.join
          (find_deps_no_transitivity instr e)
          (find_deps_offset_no_transitivity instr o)

  and find_deps_lval_no_transitivity instr lv =
    let deps,looking_for =
      !Values_To_Use.lval_to_loc_with_deps
	~with_alarms:CilE.warn_none_mode
	~skip_base_deps:false
	~deps:Zone.bottom
	instr
	lv
    in
    let looking_for = valid_enumerate_bits looking_for in
    let result = Zone.join deps looking_for in
    (*  log "find_deps_lval_no_trans:@\n deps:%a@\n looking_for:%a@\n"
	Zone.pretty deps
	Zone.pretty looking_for;*)
    result

  let find_deps instr deps_tbl expr =
    let deps_no_trans = find_deps_no_transitivity instr expr in
    !Db.From.access deps_no_trans deps_tbl

  module Computer(REACH:sig val stmt_can_reach : stmt -> stmt -> bool end) =
struct

  let empty_from =
    { additional_deps_table = StmtMap.empty;
      additional_deps = Zone.bottom;
      deps_table = Lmap_bitwise.From_Model.empty }

  let name = "from"

  let debug = ref false

  let current_stmt = ref Kglobal

  let stmt_can_reach = REACH.stmt_can_reach

  type t = t'

  module StmtStartData =
    Dataflow.StmtStartData(struct type t = t' let size = 107 end)

  type substit = Froms of Zone.t | Lvalue of Lmap_bitwise.From_Model.LOffset.t

  let cached_substitute call_site_froms extra_loc subst =
    let f k intervs =
      match k with
      | Base.Var((*{vtype = typ} as*) vi,_)
      | Base.Initialized_Var ((*{ vtype = typ } as*) vi,_)->
	  (*
            let typ_size = Bit_utils.sizeof typ in
	  if not (Int_Intervals.is_included
		    intervs
		    (Int_Intervals.inject_zero_max typ_size))
	  then
            ignore(CilE.warn_once"could be accessing argument out of bounds");*)
	  ( try
	      ( match VarinfoHashtbl.find subst vi
		with
		  Froms f -> f
		| Lvalue offs ->
		    (Lmap_bitwise.From_Model.LOffset.find_intervs
                       (Zone.default k)
                       intervs
		       offs))
	    with
	      Not_found ->
		Lmap_bitwise.From_Model.find call_site_froms (Zone.inject k intervs) )
      | Base.Null | Base.String _ | Base.Cell_class _ ->
	  Zone.inject k intervs
      

    in
    let joiner = Zone.join in
    let projection base =
      match Base.validity base with
	Base.Known (min_valid,max_valid) ->
	  Int_Intervals.inject_bounds min_valid max_valid
      | _ -> assert false (* PC TODO: il faudrait faire quelque chose dans
			     le cas "tout". Le cas "rien" ne devrait jamais
			     arriver, et s'il le fait on a le droit de
			     renvoyer bottom (hypothese que les menaces sont
			     vérifiées par ailleurs) *)
    in
    let zone_substitution =
	Zone.cached_fold ~cache:("from substitution", 331)
	  ~f ~joiner ~empty:Zone.bottom ~projection
    in
    let zone_substitution x =
      try
	zone_substitution x
      with Zone.Error_Top -> Zone.top
    in
    fun z -> Zone.join extra_loc (zone_substitution z)


  let display_one_from fmt k v =
    Format.fprintf fmt "Statement: %d@\n%a"
      k
      Lmap_bitwise.From_Model.pretty
      v.deps_table;
    Format.fprintf fmt "Additional Variable Map : %a@\n"
      (StmtMap.pretty Zone.pretty)
      v.additional_deps_table;
    Format.fprintf fmt
     "Additional Variable Map Set : %a@\n"
      Zone.pretty
      v.additional_deps

  let display_from fmt =
    Format.fprintf fmt "=========FROM START=======@\n";
    StmtStartData.iter (display_one_from fmt);
    Format.fprintf fmt "=========FROM END=======@\n"

  let copy (d: t) = d

  let pretty fmt (v: t) =
    display_one_from fmt 9999 v

  let eliminate_additional table s =
    let current_function = Stack.top call_stack in
    (* Eliminate additional variables originating
       from a branch closing at this statement. *)
    StmtMap.fold
      (fun k v (acc_set,acc_map,nb) ->
	 try
           if !Postdominators.is_postdominator
             current_function
             ~opening:k
             ~closing:s
           then acc_set,acc_map,nb
           else
             (Zone.join v acc_set),
           (StmtMap.add k v acc_map),nb+1
	 with e ->
	   Format.printf "internal error 356: (%s)Open:%d Close:%d"
	     (Printexc.to_string e) k.sid s.sid;
	   assert false
      )
      table
      (Zone.bottom, StmtMap.empty,0)

  let computeFirstPredecessor (s: stmt) data =
    let new_additional_deps, new_additional_deps_table, _ =
      eliminate_additional data.additional_deps_table s
    in
    let data =
      {data with
         additional_deps = new_additional_deps;
         additional_deps_table = new_additional_deps_table}
    in
    match s.skind with
      | Switch (exp,_,_,_)
      | If (exp,_,_,_) ->
          let additional_vars = find_deps (Kstmt s) data.deps_table exp in
          {data with
             additional_deps_table =
              StmtMap.add
                s
                additional_vars
                data.additional_deps_table;
             additional_deps =
              Zone.join
                additional_vars
                data.additional_deps }
      | _ -> data

  let combinePredecessors
      (s: stmt)
      ~old:({(*additional_deps_table = old_t;*)
             deps_table = old_table} as old)
      ({(*additional_deps_table = new_t;*)
        deps_table = new_table } as new_) =
    let new_ = computeFirstPredecessor s new_ in
    let changed = ref false in
    let merged =
      StmtMap.fold
        (fun k v acc ->
           try
             let current_val = StmtMap.find k acc.additional_deps_table in
             if Zone.is_included v current_val then
             acc
             else
               begin
                 changed := true;
                 {acc with
                    additional_deps_table =
                     StmtMap.add
                       k
                       (Zone.join current_val v)
                       acc.additional_deps_table;
                    additional_deps = Zone.join v acc.additional_deps}
               end
           with Not_found ->
             changed := true;
             {acc with
                additional_deps_table =
                 StmtMap.add
                   k
                   v
                   acc.additional_deps_table;
                additional_deps = Zone.join v acc.additional_deps
             }
        )
        new_.additional_deps_table
        old
    in
    let result = Lmap_bitwise.From_Model.join old_table new_table
    in
(*    display_from
      Format.std_formatter
      stmtStartData;*)
    if (not !changed) && Lmap_bitwise.From_Model.is_included result old_table
    then
      ((*Format.printf "STABLE RESULT:%a@\nOLD:%a@\n"
         From_Model.Memory.pretty result
         From_Model.Memory.pretty old_table;*)
        None)
    else
      ((*Format.printf "LATER RESULT(%b):%a[[%a]]@\nOLD:%a[[%a]]@\n"
         !changed
         From_Model.pretty result
         (StmtMap.pretty Zone.pretty)
         new_t
         From_Model.pretty old_table
         (StmtMap.pretty Zone.pretty)
         old.additional_deps_table;*)
       Some
         ({merged with
             deps_table = result
          }))

  let resolv_func_vinfo ?deps kinstr funcexp =
    !Values_To_Use.expr_to_kernel_function ?deps kinstr funcexp

  exception Ignore

  let doInstr _stmt (i: instr) (d: t) =
    !Db.progress ();
    let kinstr = !current_stmt
    in
    let add_with_additional_var k v d =
      (* Format.printf "VAR FOR SET: %a+%a\n"
         Zone.pretty v
         Zone.pretty d.additional_deps; *)
      let deps, looking_for =
        (* The modified tsets are [looking_for],
           those address are function of [deps]. *)
        !Values_To_Use.lval_to_loc_with_deps
          ~with_alarms:CilE.warn_none_mode
          ~skip_base_deps:false
          ~deps:Zone.bottom
          kinstr
          k
      in
      let deps = Zone.join
        v
        (Lmap_bitwise.From_Model.find d.deps_table deps)
      in
      let r = !Db.From.update
        looking_for
        (Zone.join
           d.additional_deps
           deps)
        d.deps_table

      in
      {d with deps_table=r; }
    in
    match i with
    | Set (lv, exp, _) ->
        Dataflow.Post
          (fun state ->
             let comp_vars = find_deps kinstr state.deps_table exp in
             (* Format.printf "ADD VAR FOR SET(line %d): %a\n"
                location.Cil.line Zone.pretty vars; *)
             add_with_additional_var
               lv
               comp_vars
               state)
    | Call (lvaloption,funcexp,argl,_) ->
        Dataflow.Post
          (fun state ->
             let funcexp_deps, called_vinfos =
               resolv_func_vinfo
                 ~with_alarms:CilE.warn_none_mode
                 ~deps:Zone.bottom
                 kinstr
                 funcexp
             in
             let do_on kernel_function =
               let called_vinfo = Kernel_function.get_vi kernel_function in
	       if Ast_info.is_cea_function called_vinfo.vname then
		 state
	       else
                 let { Function_Froms.deps_return = return_from;
                       deps_table = called_func_froms } =
                   Froms_To_Use.get kernel_function kinstr
                 in
		 (*		 Format.printf "Debugging From Calling %s -> return=%a@."
				 called_vinfo.vname
				 Zone.pretty return_from; *)
                 let args_froms =
                   List.map
		     (fun arg ->
                        match arg with
                          (* TODO : optimize the dependencies on subfields
                             | Lval lv ->
                             Lvalue
                             (From_Model.LBase.find
                             (Interp_loc.lval_to_loc_with_deps kinstr lv))
                          *)
                        | _ ->
		            Froms (find_deps kinstr d.deps_table arg))
		     argl
                 in
                 let formal_args =
		   Kernel_function.get_formals kernel_function
		 in
                 let name_to_from = VarinfoHashtbl.create 7 in
                 begin try
                   List.iter2
		     (VarinfoHashtbl.add name_to_from)
                     formal_args
                     args_froms;
                 with Invalid_argument "List.iter2" ->
                   ignore
                     (CilE.warn_once
                        "variadic call detected. Using only %d argument(s)."
                        (min
                           (List.length formal_args)
                           (List.length args_froms)))
                 end;
                 let funcexp_deps =
                   (* dependencies for the evaluation of [funcexp] *)
                   !Db.From.access funcexp_deps state.deps_table in
                 let additional_deps =
                   Zone.join d.additional_deps funcexp_deps
                 in
                 let substitute =
		   cached_substitute
                     state.deps_table
                     additional_deps
                     name_to_from
		 in
                 let new_state =
                   (* From state just after the call,
                      but before the result assigment *)
                   {state with
                      deps_table =
                       ((*Format.printf "map_and_merge_name: %s (%d)@."
			  called_vinfo.vname (Ast_info.get_sid kinstr);*)
			 Lmap_bitwise.From_Model.map_and_merge substitute
                           called_func_froms
                           state.deps_table)}
                 in
                 (* Treatement for the possible assignement
                    of the call result *)
                 (match lvaloption with
                  | None -> new_state
                  | Some lv ->
                      (try
                         Lmap_bitwise.From_Model.LOffset.fold
                           (fun itv (_,x) acc ->
                              let res = substitute x in
                              let deps, loc =
				!Values_To_Use.lval_to_loc_with_deps
                                  ~with_alarms:CilE.warn_none_mode
                                  ~skip_base_deps:false
                                  ~deps:Zone.bottom
                                  kinstr
                                  lv
                              in
                              let deps =
                                (Lmap_bitwise.From_Model.find acc.deps_table
                                   deps)
                              in
                              let deps = Zone.join res deps in
                              let deps = Zone.join deps acc.additional_deps in
                              let base, range =
                                Location_Bits.find_lonely_binding loc.loc
                              in let start = match Ival.min_int range with
                                None -> assert false
                              | Some i -> i
                              in
                              let zones =
                                Int_Intervals.fold
                                  (fun (lb,ub) acc ->
                                     let zone =
                                       Zone.inject base
                                         (Int_Intervals.inject
                                            [Int.add start lb,
                                             Int.add start ub])
                                     in
                                     Zone.join zone acc)
                                  itv Zone.bottom
                              in
                              let real_loc = Locations.filter_loc loc zones in
                              { acc with deps_table =
                                  !Db.From.update
                                    real_loc
                                    deps acc.deps_table}
                           )
                           return_from new_state
                       with Not_found -> (* from find_lonely_binding *)
                         let vars =
                           Lmap_bitwise.From_Model.LOffset.map
                             (fun (b,x) -> (b,substitute x))
                             return_from
                         in
                         add_with_additional_var
                           lv
                           (Lmap_bitwise.From_Model.LOffset.collapse vars)
                           new_state
                      ))
             in
             let fold_no_neutral l =
               match l with
               | [] -> state
               | h::t ->
                   let acc = do_on h (* First call *)
                   in List.fold_left (* Combine other calls *)
                        (fun acc_memory called_vinfo ->
                           let done_on = do_on called_vinfo in
                           {state with
                              deps_table = Lmap_bitwise.From_Model.join
                               done_on.deps_table
                               acc_memory.deps_table})
                        acc
                        t
             in 
	     try
	       fold_no_neutral called_vinfos
	     with Call_did_not_take_place -> state
          )
    | _ -> Dataflow.Default

  let doStmt (s: stmt) (_d: t) =
    if not (Db.Value.is_reachable (Values_To_Use.get_state (Kstmt s))) then
      Dataflow.SDone
    else begin
      current_stmt := Kstmt s;
      Dataflow.SDefault
    end

  let filterStmt stmt =
    Db.Value.is_reachable (Values_To_Use.get_state (Kstmt stmt))

  (* Remove all local variables and formals from table *)
  let externalize return fundec state =
    let deps_return = (match return.skind with
                         | Return (Some (Lval v),_) ->
                             let deps, looking_for =
                               !Values_To_Use.lval_to_loc_with_deps
                                 ~with_alarms:CilE.warn_none_mode
				 ~deps:Zone.bottom
                                 ~skip_base_deps:false
                                 (Kstmt return)
                                 v
                             in
                             Lmap_bitwise.From_Model.LOffset.join
                               (Lmap_bitwise.From_Model.find_base
                                  state.deps_table deps)
                               (Lmap_bitwise.From_Model.find_base
                                  state.deps_table
                                  (valid_enumerate_bits looking_for))
(*
                               (!Db.From.access deps state.deps_table)
                               (!Db.From.access (valid_enumerate_bits looking_for) state.deps_table)
*)
                         | Return (None,_) ->
                             Lmap_bitwise.From_Model.LOffset.empty
                         | _ -> assert false)
    in
    let deps_table = Lmap_bitwise.From_Model.filter_base
      (fun v -> not (Base.is_formal_or_local v fundec))
      state.deps_table
    in
    { deps_return = deps_return;
      Function_Froms.deps_table = deps_table }

  let doGuard s e _t =
    let ki = Kstmt s in
    current_stmt := ki;
    let do_it =
      let t1 = unrollType (typeOf e) in
      if isIntegralType t1 || isPointerType t1
      then Cvalue_type.V.contains_non_zero (!Values_To_Use.access_expr ki e)
      else true (* TODO: a float condition is true iff != 0.0 *)
    in
    if do_it
    then Dataflow.GDefault
    else Dataflow.GUnreachable
end

let compute_using_cfg kf = match kf.fundec with
  | Declaration _ -> assert false
  | Definition (f,_) ->
      try
        let module Computer =
          Computer
	    (struct let stmt_can_reach = Stmts_graph.stmt_can_reach kf end)
        in
        let module Compute = Dataflow.ForwardsDataFlow(Computer) in

        Stack.iter
          (fun g ->
             if kf == g then begin
             error
               "ignoring recursive call detected in function %a during dependencies computations."
               Kernel_function.pretty_name kf;
               raise Exit
             end)
          call_stack;
        Stack.push kf call_stack;
        match f.sbody.bstmts with
          [] -> assert false
        | start :: _ ->
            let ret_id = Kernel_function.find_return kf in
            (* Format.printf "Return is %d@\n\n" ret_id; *)
            (* We start with only the start block *)
            Computer.StmtStartData.add
              start.sid
              (Computer.computeFirstPredecessor
                 start
                 Computer.empty_from);
            Compute.compute [start];
            let _poped = Stack.pop call_stack in
            (*Format.printf "[from] poped %s\n@\n" (get_name poped);
              Stack.iter
              (fun g ->
              Format.printf "[from] stack with %s\n@\n" (get_name g))
              call_stack;*)

            let last_from =
              try
		if Db.Value.is_reachable (Values_To_Use.get_state (Kstmt ret_id))
		then
                  Computer.externalize
                  ret_id
                  f
                  (Computer.StmtStartData.find ret_id.sid)
		else
		  raise Not_found
              with Not_found -> begin
                log "Non terminating function (no dependencies)@\n";
                { Function_Froms.deps_return =
                    Lmap_bitwise.From_Model.LOffset.empty;
                  deps_table = Computer.empty_from.deps_table }
              end
            in
	    last_from

      with Exit ->
          { Function_Froms.deps_return = Lmap_bitwise.From_Model.LOffset.empty;
	    deps_table = Lmap_bitwise.From_Model.empty }

let compute_using_prototype_for_state state kf =
  match kf.fundec with
  | Definition _ -> assert false
  | Declaration (_, varinfo, _,_) ->
      let behaviors = !Value.valid_behaviors kf state in
      let assigns = Ast_info.merge_assigns behaviors in
      let return_deps,deps =
        match assigns with
          [] ->
	    Lmap_bitwise.From_Model.LOffset.empty,
            Lmap_bitwise.From_Model.empty
	| assigns ->
            let (rt_typ,_,_,_) = Cil.splitFunctionTypeVI varinfo in
	    let input_zone ins =
	      match ins with
		  [] -> Zone.top
                | l ->
                    (try
                       List.fold_left
                         (fun acc loc ->
                            List.fold_left
                              (fun acc lv ->
		                 Zone.join acc
			           (!Value.lval_to_zone_state state lv))
                              acc
                              (match loc with
                                   Location loc ->
                                     !Properties.Interp.tsets_to_lval
                                       loc.its_content
                                 | Nothing -> []
                              ))
                         Zone.bottom l
                     with Invalid_argument "not a lvalue" ->
                       CilE.warn_once "Unable to extract precise FROM in %a"
                         Kernel_function.pretty_name kf;
                       Zone.top)
	    in
	    let treat_assign acc (out, ins) =
              try
                List.fold_left
                  (fun acc lv ->
	             let output_zone =
                       try
                         !Value.lval_to_zone_state state lv
                       with Invalid_argument "not a lvalue" ->
                         CilE.warn_once "Unable to extract assigns in %a"
                           Kernel_function.pretty_name kf;
                         Zone.top
                     in
	             Lmap_bitwise.From_Model.add_binding ~exact:true
		       acc output_zone (input_zone ins))
                  acc
                  (match out with
                       Location out ->
                         !Properties.Interp.tsets_to_lval out.its_content
                     | Nothing -> []
                  )
              with Invalid_argument "not a lvalue" ->
                CilE.warn_once "Unable to extract assigns in %a"
                  Kernel_function.pretty_name kf;
                acc

	    in
            let treat_ret_assign acc (out,ins) =
              try
                let coffs =
                  match out with
                      Location out ->
                        !Properties.Interp.tsets_to_offset out.its_content
                    | Nothing -> []
                in
                List.fold_left
                  (fun acc coff ->
                     let (base,width) = Cil.bitsOffset rt_typ coff in
                     Lmap_bitwise.From_Model.LOffset.add_iset
                       ~exact:true
                       (Abstract_value.Int_Intervals.from_ival_size
                          (Ival.of_int base)
                          (Int_Base.inject (Int.of_int width)))
                       (input_zone ins) acc)
                  acc coffs
              with Invalid_argument "not a lvalue" | SizeOfError _ ->
                ignore
                  (CilE.warn_once "Unable to extract a proper offset. \
                                     Using FROM for the whole \\result");
                Lmap_bitwise.From_Model.LOffset.add_iset ~exact:false
                  (Abstract_value.Int_Intervals.from_ival_size
                     (Ival.of_int 0) (Bit_utils.sizeof rt_typ))
                  (input_zone ins) acc
            in
            let return_assigns, other_assigns =
              List.fold_left
                (fun (ra,oa as res) (x,_ as a) ->
                   match x with
                       Location loc ->
                         if Logic_const.tsets_is_result loc.its_content
                         then a::ra,oa else ra,a::oa
                     | Nothing -> res
                )
                ([],[]) assigns
            in
            (List.fold_left treat_ret_assign
               Lmap_bitwise.From_Model.LOffset.empty return_assigns,
	     List.fold_left
	       treat_assign
	       Lmap_bitwise.From_Model.empty
	       other_assigns)
      in
      { deps_return = return_deps; Function_Froms.deps_table = deps }

let compute_using_prototype kf =
  let state = Value.get_initial_state kf in
  compute_using_prototype_for_state state kf

let compute_and_return kf =
  let call_site_loc = !currentLoc in
  log
    "[from] computing for function %a%s@\n@?"
    Kernel_function.pretty_name kf
    (let s = ref "" in
     Stack.iter
       (fun kf ->
	  s := !s^" <-"^(fprintf_to_string "%a" Kernel_function.pretty_name kf))
       call_stack;
     !s);

  let result = match kf.fundec with
    | Definition _ ->
        compute_using_cfg kf
    | Declaration _ ->
        compute_using_prototype kf
  in
  Recording_To_Do.record_kf kf result;
  log "[from] done for function %a@\n@?" Kernel_function.pretty_name kf;
  currentLoc := call_site_loc;
  result

let compute kf =
  !Db.Value.compute ();
  ignore (compute_and_return kf)

end

(* Application-wide Froms *)

module Functionwise_From_to_use =
struct
  let compute = Db.From.compute
  let memo kf =
    Functionwise_Dependencies.memo
      (fun kf ->
	 !compute kf;
         try Functionwise_Dependencies.find kf
	 with Not_found -> invalid_arg "could not compute dependencies")
      kf
  let get kf _ = memo kf
end

module Recording_To_Do =
struct
  let record_kf kf last_from = Functionwise_Dependencies.add kf last_from
end

module From2 = Make(Db.Value)(Functionwise_From_to_use)(Recording_To_Do)

let () =
  Db.From.compute := From2.compute;
  Db.From.get := Functionwise_From_to_use.memo

let () = Db.From.pretty :=
  (fun fmt v ->
     let (*{deps_return = ret; Function_Froms.deps_table = fr} *) deps =
       Functionwise_From_to_use.memo v in
     Function_Froms.pretty_with_type (Kernel_function.get_type v) fmt deps)
(*
     if Db.returns_void v
     then
       Format.fprintf fmt "@[<v>@[@;<2 0>@[%a@]@]@ @]"
         Lmap_bitwise.From_Model.pretty fr
     else
       let (rt_typ,_,_,_) = Cil.splitFunctionType (Db.get_type v) in
       Format.fprintf fmt "@[<v>@[@;<2 0>@[%a@]\\result@[%a@]@]@\n"
         Lmap_bitwise.From_Model.pretty fr
         (Lmap_bitwise.From_Model.LOffset.pretty_with_type (Some rt_typ)) ret)
*)

let () = Db.From.find_deps_no_transitivity := From2.find_deps_no_transitivity

(* Call Froms *)

let merge_call_froms table callsite froms =
  try
    let current = InstrHashtbl.find table callsite in
    let new_froms = Function_Froms.join froms current in
    InstrHashtbl.replace table callsite new_froms
  with Not_found ->
    InstrHashtbl.add table callsite froms

let call_froms_stack = ref []

let record_callwise_dependencies_in_db call_site froms =
  try
    let previous = Callwise_Dependencies.find call_site
    in
    Callwise_Dependencies.replace
      call_site
      (Function_Froms.join previous froms)
  with Not_found ->
    Callwise_Dependencies.add call_site froms

let call_for_individual_froms (state, call_stack) =
  if Cmdline.ForceCallDeps.get ()
  then begin
    let current_function, call_site = List.hd call_stack in
    match current_function.fundec with
      Definition _ ->
	let table_for_current_function = InstrHashtbl.create 7 in
	call_froms_stack :=
	  (current_function,table_for_current_function) :: !call_froms_stack
    | Declaration _ ->
	( try
	    let _above_function, table = List.hd !call_froms_stack in
	    let froms =
	      From2.compute_using_prototype_for_state
		state current_function
	    in
	    merge_call_froms table call_site froms;
	    record_callwise_dependencies_in_db call_site froms;
	  with Failure "hd" ->
	    Format.printf "calldeps internal error 23 empty callfromsstack %a@."
	      Kernel_function.pretty_name current_function
	)
  end

let record_for_individual_froms (call_stack, instrstates) =
  if Cmdline.ForceCallDeps.get ()
  then begin
    let module Froms_To_Use =
	struct
	  let get _f callsite =
	    let _current_function, table = List.hd !call_froms_stack in
(*	    match f.fundec with
	      Definition _ -> *)
		begin try
		    InstrHashtbl.find table callsite
		  with Not_found ->
(*		    Format.printf "calldeps: call did not take place %a calling %a@."
		      Kernel_function.pretty_name _current_function
		      Kernel_function.pretty_name _f;*)
		    raise Call_did_not_take_place
		    
		end
(*	    | Declaration _ ->
		Functionwise_From_to_use.get f callsite *)
	end
    in
    let module Values_To_Use =
	struct
	  let get_state k =
            try InstrHashtbl.find instrstates k
	    with Not_found -> Relations_type.Model.bottom

      (* TODO: This should be better factored with Kinstr ! *)
	  let lval_to_loc_with_deps kinstr ~with_alarms:_
	      ~skip_base_deps ~deps lv =
	    let state = get_state kinstr in
            !Db.Value.lval_to_loc_with_deps_state state ~skip_base_deps
	      ~deps lv

      let lval_to_loc_with_deps = ref lval_to_loc_with_deps

      let expr_to_kernel_function kinstr ~with_alarms:_ ~deps exp =
	let state = get_state kinstr in
	!Db.Value.expr_to_kernel_function_state state ~deps exp

      let expr_to_kernel_function = ref expr_to_kernel_function

      let access_expr kinstr expr =
	let state = get_state kinstr in
	!Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state expr
      let access_expr = ref access_expr
    end
    in
    let module Recording_To_Do =
      struct
	let record_kf _kf _last_froms = ()
      end
    in
    let module Callwise_Froms =
      Make(Values_To_Use)(Froms_To_Use)(Recording_To_Do)
    in
    let current_function, call_site = List.hd call_stack in
    let froms = Callwise_Froms.compute_and_return current_function in
(*    Format.printf "At call site %s, dependencies of %s:@.%a@."
      (match call_site with
	Kglobal -> "Kglobal"
      | Kstmt s -> string_of_int s.sid) 
      (Kernel_function.get_name current_function)
      Function_Froms.pretty froms;  *)
    record_callwise_dependencies_in_db call_site froms;
    (* pop + record in top of stack the froms of function that just finished *)
    match !call_froms_stack with
      (current_function2, _) :: (((_caller, table) :: _) as tail) ->
	    assert (
	      if current_function2 != current_function then begin
		Format.printf "calldeps %a != %a@."
		  Kernel_function.pretty_name current_function (* g *)
		  Kernel_function.pretty_name current_function2; (* f *)
		false
	      end else
		true);
	call_froms_stack := tail;
	merge_call_froms table call_site froms

    | _ -> () (* the entry point, probably *)


  end

let () =
  Options.add_plugin 
    ~name:"functional dependencies"
    ~descr:""
    [ "-deps", Arg.Unit Cmdline.ForceDeps.on, ": force dependencies display";
      "-calldeps", Arg.Unit Cmdline.ForceCallDeps.on, 
      ": force callsite-wise dependencies (through value analysis)" ]

let () =
(*  if Cmdline.ForceCallDeps.get ()
  then *) begin
    Db.Value.Record_Value_Callbacks.extend record_for_individual_froms;
    Db.Value.Call_Value_Callbacks.extend call_for_individual_froms
  end

let find_available kinstr =
  if Cmdline.ForceCallDeps.get ()
  then begin
      try
	Callwise_Dependencies.find kinstr
      with Not_found ->
	Format.printf "internal error 457 : From.find_available called on non-Call statement, or called too early.";
	assert false
    end
  else begin
      match kinstr with
      | Kstmt ({skind = Instr(Call (_,funcexp,_,_))}) ->
	  let _, called_functions =
	    !Value.expr_to_kernel_function
	      ~with_alarms:CilE.warn_none_mode
	      kinstr ~deps:None funcexp
	  in
	  let treat_kf _kf acc =
	    let kf_froms = (assert false) 
	    in
	    match acc with
	      None -> Some kf_froms
	    | Some froms -> 
		Some (Function_Froms.join kf_froms froms)
	  in
	  let froms = 
	    List.fold_right treat_kf called_functions None
	  in
	  begin
	    match froms with
	      None -> assert false (* TODO: do something *)
	    | Some f -> f 
	  end
      | _ ->
	  Format.printf "internal error 458 : From.find_available called on non-Call statement.";
	assert false
    end

let () =
  Db.From.Callwise.iter := Callwise_Dependencies.iter ;
  Db.From.Callwise.find := Callwise_Dependencies.find

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
