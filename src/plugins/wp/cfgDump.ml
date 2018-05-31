(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(*                                                                        *)
(**************************************************************************)

let _dkey = "cfgdump" (* debugging key *)

module VC =
struct

  let fc = ref None
  let out = ref Format.std_formatter
  let knode = ref 0
  let node () = incr knode ; !knode

  let create kf bhv =
    begin
      let name =
        match bhv with
        | None -> Kernel_function.get_name kf
        | Some bname -> Kernel_function.get_name kf ^ "_" ^ bname
      in
      let file = Filename.concat (Wp_parameters.get_output ()) name in
      Wp_parameters.feedback "CFG %a -> %s@." Kernel_function.pretty kf name ;
      let fout = open_out (file ^ ".dot") in
      fc := Some (fout,file) ;
      out := Format.formatter_of_out_channel fout ;
      Format.fprintf !out "digraph %a {@\n" Kernel_function.pretty kf ;
      Format.fprintf !out "  rankdir = TB ;@\n" ;
      Format.fprintf !out "  node [ style = filled, shape = box ] ;@\n" ;
      Format.fprintf !out "  N000 [ color = red, shape = circle, label = \"*\" ] ;@\n" ;
    end

  let flush () =
    begin
      Format.fprintf !out "}@." ;
      out := Format.std_formatter ;
      match !fc with
      | None -> ()
      | Some (fout,file) ->
          close_out fout ;
          ignore (Sys.command
                    (Printf.sprintf "dot -Tpdf %s.dot > %s.pdf" file file))
    end

  (* -------------------------------------------------------------------------- *)
  (* --- MCFG Interface                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  type t_prop = int (* current node *)

  let pretty fmt k = Format.fprintf fmt "N%03d" k

  let link a b =
    if b =0
    then Format.fprintf !out " %a -> %a [ style=dotted ];@." pretty a pretty b
    else Format.fprintf !out " %a -> %a ;@." pretty a pretty b

  let merge _env k1 k2 =
    if k1=0 then k2 else
    if k2=0 then k1 else
      let u = node () in
      Format.fprintf !out "  %a [ label=\"\" , shape=circle ] ;@." pretty u ;
      link u k1 ; link u k2 ; u

  let empty = 0

  let has_init _ = false

  type t_env = Kernel_function.t

  let new_env ?lvars kf : t_env = ignore lvars ; kf

  let add_axiom _p _l = ()

  let add_hyp _env (pid,pred) k =
    let u = node () in
    if Wp_parameters.debug_atleast 1 then
      Format.fprintf !out "  %a [ color=green , label=\"Assume %a\" ] ;@." pretty u Printer.pp_predicate pred
    else
      Format.fprintf !out "  %a [ color=green , label=\"Assume %a\" ] ;@." pretty u WpPropId.pp_propid pid ;
    link u k ; u

  let add_goal env (pid,pred) k =
    let u = node () in
    if Wp_parameters.debug_atleast 1 then
      Format.fprintf !out "  %a [ color=red , label=\"Prove %a\" ] ;@." pretty u Printer.pp_predicate pred
    else
      Format.fprintf !out "  %a [ color=red , label=\"Prove %a\" ] ;@." pretty u WpPropId.pp_propid pid ;
    Format.fprintf !out "  %a -> %a [ style=dotted ] ;@." pretty u pretty k ;
    merge env u k

  let pp_assigns fmt = function
    | Cil_types.WritesAny -> Format.pp_print_string fmt " \\everything"
    | Cil_types.Writes [] -> Format.pp_print_string fmt " \\nothing"
    | Cil_types.Writes froms ->
        List.iter
          (fun (t,_) -> Format.fprintf fmt "@ %a" Printer.pp_identified_term t)
          froms

  let add_assigns env (pid,_) k =
    let u = node () in
    Format.fprintf !out "  %a [ color=red , label=\"Assigns %a\" ] ;@." pretty u WpPropId.pp_propid pid ;
    merge env u k

  let use_assigns _env _stmt region d k =
    let u = node () in
    begin match region with
      | None ->
          Format.fprintf !out "  %a [ color=orange , label=\"Havoc All\" ] ;@." pretty u
      | Some pid ->
          Format.fprintf !out "  %a [ color=orange , label=\"Havoc %a:\n@[<hov 2>assigns%a;@]\" ] ;@."
            pretty u WpPropId.pp_propid pid
            pp_assigns d.WpPropId.a_assigns
    end ;
    link u k ; u

  let label _env stmt label k =
    if Clabels.is_here label then k else
      let u = node () in
      ( match stmt with
        | None ->
            Format.fprintf !out "  %a [ label=\"Label %a\" ] ;@." pretty u Clabels.pretty label
        | Some s ->
            Format.fprintf !out "  %a [ label=\"Label %a (Stmt s%d)\" ] ;@." pretty u Clabels.pretty label s.Cil_types.sid
      ) ;
      link u k ; u

  let assign _env _stmt x e k =
    let u = node () in
    Format.fprintf !out "  %a [ color=orange , label=\"%a := %a\" ] ;@." pretty u
      Printer.pp_lval x Printer.pp_exp e ;
    link u k ; u

  let return _env _stmt r k =
    let u = node () in
    begin
      match r with
      | None ->
          Format.fprintf !out "  %a [ color=orange , label=\"Return\" ] ;@." pretty u
      | Some e ->
          Format.fprintf !out "  %a [ color=orange , label=\"Return %a\" ] ;@." pretty u
            Printer.pp_exp e
    end ;
    link u k ; u

  let test _env _stmt e k1 k2 =
    let u = node () in
    Format.fprintf !out "  %a [ color=cyan , label=\"If %a\" ] ;@." pretty u Printer.pp_exp e ;
    link u k1 ; link u k2 ; u

  let switch _env _stmt e cases def =
    let u = node () in
    Format.fprintf !out "  %a [ color=cyan , label=\"Switch %a\" ] ;@." pretty u Printer.pp_exp e ;
    List.iter (fun (_,k) -> link u k) cases ;
    link u def ; u

  let const _ x k =
    let u = node () in
    Format.fprintf !out "  %a [ color=orange, label=\"const %a\" ] ;@."
      pretty u Printer.pp_lval (Cil.var x) ;
    link u k ; u

  let init _ x init k =
    let u = node () in
    let pp_init fmt = function
      | None -> Format.pp_print_string fmt "<default>"
      | Some init -> Printer.pp_init fmt init
    in
    Format.fprintf !out "  %a [ color=orange, label=\"init %a := %a\" ] ;@."
      pretty u Printer.pp_lval (Cil.var x) pp_init init ;
    link u k ; u

  let tag s k =
    let u = node () in
    Format.fprintf !out "  %a [ color=cyan , label=\"Tag %s\" ] ;@." pretty u s ;
    link u k ; u

  let loop_entry w = tag "BeforeLoop" w
  let loop_step w = tag "InLoop" w

  let call_dynamic _env _stmt _pid fct calls =
    let u = node () in
    Format.fprintf !out "  %a [ color=red , label \"CallPtr %a\" ];@." pretty u
      Printer.pp_exp fct ;
    List.iter (fun (_,k) -> link u k) calls ; u

  let call_goal_precond env _stmt kf _es ~pre k =
    let u = node () in
    Format.fprintf !out "  %a [ color=red , label=\"Prove PreCond %a%t\" ] ;@."
      pretty u Kernel_function.pretty kf
      begin fun fmt ->
        if Wp_parameters.debug_atleast 1 then
          List.iter
            (fun (_,p) -> Format.fprintf fmt "\n@[<hov 2>Requires %a ;@]"
                Printer.pp_predicate p) pre
      end ;
    ignore pre ; merge env u k
  
  let call env stmt _r kf _es ~pre ~post ~pexit ~assigns ~p_post ~p_exit =
    let u_post = List.fold_right (add_hyp env) post p_post in
    let u_exit = List.fold_right (add_hyp env) pexit p_exit in
    let u = node () in
    link u u_post ; link u u_exit ;
    Format.fprintf !out
      "  %a [ color=orange , label=\"Call %a @[<hov 2>(assigns%a)@]\" ] ;@."
      pretty u Kernel_function.pretty kf pp_assigns assigns ;
    ignore stmt ;
    List.fold_right (add_hyp env) pre u

  let pp_scope sc fmt xs =
    let title = match sc with
      | Mcfg.SC_Global -> "Global"
      | Mcfg.SC_Function_in -> "F-in"
      | Mcfg.SC_Function_frame -> "F-frame"
      | Mcfg.SC_Function_out -> "F-out"
      | Mcfg.SC_Block_in -> "B-in"
      | Mcfg.SC_Block_out -> "B-out"
    in begin
      Format.fprintf fmt "%s {" title ;
      List.iter (fun x -> Format.fprintf fmt " %a" Printer.pp_varinfo x) xs ;
      Format.fprintf fmt " }" ;
    end

  let scope _kfenv xs scope k =
    let u = node () in
    Format.fprintf !out "  %a [ color=lightblue , label=\"%a\" ] ;@." pretty u
      (pp_scope scope) xs ;
    link u k ; u

  let close kfenv k =
    let u = node () in
    Format.fprintf !out "  %a [ color=cyan , label=\"Function %a\" ] ;@." pretty u
      Kernel_function.pretty kfenv ;
    link u k ; u

  let build_prop_of_from _env _ps _k = 0

end

module WP = Calculus.Cfg(VC)

(* ------------------------------------------------------------------------ *)
(* --- Proof Obligation Generation                                      --- *)
(* ------------------------------------------------------------------------ *)

class computer () =
  let driver = Driver.load_driver () in
  let model = Factory.(instance default driver) in
  object
    val mutable wptasks = []

    method model = model
    method lemma = true
    method add_lemma (_ : LogicUsage.logic_lemma) = ()

    method add_strategy strategy =
      wptasks <- strategy :: wptasks

    method compute : Wpo.t Bag.t =
      begin

        (* Generates Wpos and accumulate exported goals *)
        List.iter
          (fun strategy ->
             let cfg = WpStrategy.cfg_of_strategy strategy in
             let kf = Cil2cfg.cfg_kf cfg in
             let bhv = WpStrategy.behavior_name_of_strategy strategy in
             VC.create kf bhv ;
             try ignore (WP.compute cfg strategy) ; VC.flush ()
             with err -> VC.flush () ; raise err
          ) wptasks ;
        wptasks <- [] ;
        Bag.empty

      end (* method compute *)

  end (* class computer *)

let create () = (new computer () :> Generator.computer)
