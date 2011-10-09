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

open Cil_types

type dnode = {
  dn_id : int;
  dn_warn : Wpo.warning list;
  dn_depends : Property.t list;
  dn_source : dnode list;
}

module Sint = Set.Make
  (struct type t = int let compare = Datatype.Int.compare end)

let iter_dnodes f d =
  let rec iter marked f d =
    if not (Sint.mem d.dn_id !marked) then
      ( (f d:unit) ; marked := Sint.add d.dn_id !marked ;
        List.iter (iter marked f) d.dn_source )
  in iter (ref Sint.empty) f d

let pp_list pp fmt xs =
  List.iter (fun x -> pp fmt x ; Format.pp_print_newline fmt ()) xs

let pp_dnode fmt d =
  iter_dnodes
    (fun d ->
       pp_list Wpo.pp_warning fmt d.dn_warn ;
       pp_list Wpo.pp_depend fmt d.dn_depends ;
    ) d

module Create (W : Mcfg.S) =
struct

  type t_env = W.t_env
  let new_env = W.new_env

  type description = dnode

  type t_goal = {
    g_id : WpPropId.prop_id ;
    g_prop : W.t_prop;
    g_descr : dnode;
  }

  type t_prop = t_goal list

  let pp_goal fmt name g =
    begin
      Format.fprintf fmt "@[<v 0>Proof Obligation %a:@]@\n" WpPropId.pp_id_name g.g_id ;
      pp_dnode fmt g.g_descr ;
      Format.fprintf fmt "@[<v 2>Goal %s:@ %a@]@." name W.pretty g.g_prop ;
    end

  let pp_descr fmt g =
    Format.fprintf fmt "Proof Obligation for %a:@\n"
      WpPropId.pretty g.g_id ;
    pp_dnode fmt g.g_descr

  let iter_description fwrn fdep d =
    iter_dnodes
      (fun d ->
         List.iter fwrn d.dn_warn ;
         List.iter fdep d.dn_depends ;
      ) d

  let pp_goalx fmt g = pp_goal fmt "" g

  let pretty = pp_list pp_goalx

  let empty = []

  let dnode_cpt = ref 0

  let make_goal prop_id process dsource =
    let collect = Datalib.Collector.push () in
    let p =
      try process ()
      with e ->
        ignore (Datalib.Collector.pop collect) ;
        (* TODO : catch the exception here to not break the wp calculus *)
        raise e
    in
    let warns, depends = Datalib.Collector.pop collect in
    let id = incr dnode_cpt; !dnode_cpt in
    let dn = {
      dn_id = id ;
      dn_warn = warns ;
      dn_depends = depends ;
      dn_source = dsource ;
    } in {
      g_id = prop_id ;
      g_prop = p ;
      g_descr = dn ;
    }

  let rec merge goals1 goals2 =
    (* List.merge sort_obligs opl1 opl2 : no, because keeps duplicates *)
    match goals1, goals2 with
      | _, [] -> goals1
      | [], _ -> goals2
      | g1::tl1, g2::tl2 ->
          let cmp = WpPropId.compare_prop_id g1.g_id g2.g_id in
            if cmp < 0 then g1::(merge tl1 goals2)
            else if cmp > 0 then g2::(merge goals1 tl2)
            else
              let g = make_goal g1.g_id
                        (fun () -> W.merge g1.g_prop g2.g_prop)
                        [g1.g_descr; g2.g_descr]
              in g::(merge tl1 tl2)

  let add_hyp env h goals =
    let f p () = W.add_hyp env h p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let build_prop_of_from env pre goals =
    let f p () = W.build_prop_of_from env pre p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let add_goal env g goals =
    let new_prop () = W.add_goal env g W.empty in
    let g = make_goal (WpPropId.pred_info_id g) new_prop [] in
    merge [g] goals

  let add_axiom id name labels axiom  =
    let collect = Datalib.Collector.push () in
    W.add_axiom id name labels axiom;
    let warns, depends = Datalib.Collector.pop collect in
    begin
      List.iter
        (fun w ->
           Wp_parameters.warning "Warning for Axiom %s:@\nFrom %s: %s@\nEffect: %s"
             name w.Wpo.wrn_source w.Wpo.wrn_reason w.Wpo.wrn_effect)
        warns ;
      List.iter
        (fun d ->
           Wp_parameters.warning "Warning for Axiom %s:@\nDepends on %a"
             name Description.pp_property d)
        depends ;
    end

  let add_assigns env assigns goals =
    let f () = W.add_assigns env assigns W.empty in
    let new_goal = make_goal (WpPropId.assigns_info_id assigns) f [] in
    if new_goal.g_prop = W.empty then goals
    else merge [new_goal] goals

  let assigns_method = W.assigns_method

  let init_value env lv ty e_opt goals =
    let f p () = W.init_value env lv ty e_opt p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let init_range env lv ty ka kb goals =
    let f p () = W.init_range env lv ty ka kb p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let assign env lv e goals =
    let f p () = W.assign env lv e p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let return env e goals =
    let f p () = W.return env e p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let test env c goals_t goals_f =
    let test pt pf () = W.test env c pt pf in
    let rec merge lt lf = match lt, lf with
    | [], [] -> []
    | g::lt, [] ->
        let dsource = [g.g_descr] in
        let g = make_goal g.g_id (test g.g_prop W.empty) dsource in
          g::(merge lt lf)
    | [], g::lf ->
        let dsource = [g.g_descr] in
        let g = make_goal g.g_id (test W.empty g.g_prop) dsource in
          g::(merge lt lf)
    | gt::tlt, gf::tlf ->
        let cmp =  WpPropId.compare_prop_id gt.g_id gf.g_id in
          if cmp < 0 then
            let dsource = [gt.g_descr] in
            let g = make_goal gt.g_id (test gt.g_prop W.empty) dsource in
              g::(merge tlt lf)
          else if cmp > 0 then
            let dsource = [gf.g_descr] in
            let g = make_goal gf.g_id (test W.empty gf.g_prop) dsource in
              g::(merge lt tlf)
          else
            let dsource = [gt.g_descr; gf.g_descr] in
            let g = make_goal gf.g_id (test gt.g_prop gf.g_prop) dsource in
              g::(merge tlt tlf)
    in merge goals_t goals_f

  (** merge the switch branches :
  * @param e : switch expression,
  * @param cases : a list of (case expression, wp for that case),
  * @param p_def : wp for the default branch.
  * Because each wp is a list, it is not so easy to merge.
  * So we decide to chose a simple, but not optimized, algorithm :
  * - we first collect a sorted list of all the ids in every branches,
  * - we then process each id but getting the wp for this id in each branch,
  * - and we then put back things together.
  *)
  let switch env e cases p_def =
    let rec add_id ids new_id = match ids with [] -> [new_id]
      | id::other_ids -> let cmp =  WpPropId.compare_prop_id new_id id in
          if cmp = 0 then ids (* new_id already in *)
          else if cmp < 0 then new_id::ids
          else id::(add_id other_ids new_id)
    in
    let collect_id ids g = add_id ids g.g_id in
    let collect_ids ids (_, lp) = List.fold_left collect_id ids lp in
    let ids = List.map (fun g -> g.g_id) p_def in
    let ids = List.fold_left collect_ids ids cases in
      (* we now have all the ids found in all the lists *)
    let get_p_id id goals =
      try
        let g =
          List.find (fun g -> WpPropId.compare_prop_id id g.g_id = 0) goals
        in g.g_prop
      with Not_found -> W.empty
    in
    let get_descr_id id =
      let add_goal acc g =
        if WpPropId.compare_prop_id id g.g_id = 0 then g.g_descr::acc
        else acc
      in
      let add_goals = List.fold_left add_goal in
      let acc = add_goals [] p_def in
        List.fold_left (fun acc (_, gs) -> add_goals acc gs) acc cases
    in
    let process_id id =
      let id_p_def = get_p_id id p_def in
      let id_cases =
        List.map (fun (cond, lp) -> (cond, get_p_id id lp)) cases
      in
      let f () = W.switch env e id_cases id_p_def in
      let d = get_descr_id id in
        make_goal id f d
    in List.map process_id ids (* ids are sorted => goals are also sorted *)

  (* -------------------------------------------------------------------------- *)
  (* --- Call Rules                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let call_goal_precond wenv stmt kf args ~pre goals =
    let new_prop p () =
      W.call_goal_precond wenv stmt kf args ~pre:[p] W.empty
    in
    let preconds =
      List.map 
        (fun p -> make_goal (WpPropId.pred_info_id p) (new_prop p) []) pre
    in
    merge preconds goals

  let call wenv stmt lv kf args ~pre ~post ~pexit ~assigns ~p_post ~p_exit =
    let wp ~post ~pexit p_post p_exit () =
      W.call wenv stmt lv kf args
        ~pre ~post ~pexit ~assigns ~p_post ~p_exit
    in
    let g_post =
      List.map
        (fun g ->
           make_goal g.g_id (wp ~post ~pexit:[] g.g_prop W.empty)
             [g.g_descr])
        p_post
    in
    let g_exit =
      List.map
        (fun g ->
           make_goal g.g_id (wp ~post:[] ~pexit W.empty g.g_prop)
             [g.g_descr])
        p_exit
    in
    merge g_post g_exit

  let use_assigns env id assigns goals =
    let f p () = W.use_assigns env id assigns p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let label env l goals =
    let f p () = W.label env l p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let scope env vars sc goals =
    let f p () = W.scope env vars sc p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let close env goals =
    let f p () = W.close env p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

  let tag t goals =
    let f p () = W.tag t p in
    List.map (fun g -> make_goal g.g_id (f g.g_prop) [g.g_descr]) goals

end
