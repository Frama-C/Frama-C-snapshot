open Db;;
open Cil_types;;

let pp_nodes msg nodes =
  Kernel.result "%s" msg ;
  List.iter (fun n -> Kernel.result "%a" (!Pdg.pretty_node false) n) nodes;;

exception Find of varinfo;;

let main _ =
  let f = Globals.Functions.find_by_name "f" in
  let pdg = !Pdg.get f in

  (* Uncomment to retrieve sid *)
  (*Kernel.Debug.set 1;;
    Format.eprintf "@[%a@]@." Printer.pp_global (Kernel_function.get_global f);;
  *)
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  let stmt_1 = fst (Kernel_function.find_from_sid 1) in (* y = 0 *)
  let node = !Pdg.find_stmt_node pdg stmt_1 in
  let nodes = !Pdg.all_uses pdg [node] in
  pp_nodes "Test [all_uses] stmt1" nodes;

  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  let y =
    try
      Globals.Vars.iter (fun v _ -> if v.vname = "y" then raise (Find v));
      assert false
    with Find v ->
      v
  in

  let y_zone = 
    Locations.enumerate_valid_bits
      ~for_writing:false 
      (Locations.loc_of_varinfo y) 
  in
  let y_at_11_nodes, undef = (* y=5 *)
    !Pdg.find_location_nodes_at_stmt
      pdg (fst (Kernel_function.find_from_sid 11)) ~before:false y_zone
  in

  assert (undef = None);
  let y_at_11_nodes = List.map (fun (n,_z) -> n) y_at_11_nodes in

  let () = pp_nodes "Test [find_location_nodes_at_stmt] y@11" y_at_11_nodes in
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  let nodes = !Pdg.all_dpds pdg y_at_11_nodes in
  let () = pp_nodes "Test [all_dpds] y@11" nodes in
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  let nodes = !Pdg.all_uses pdg y_at_11_nodes in
  let () = pp_nodes "Test [all_uses] y@11" nodes in
  (*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
  let all_related_nodes pdg =
    let all n = (!Pdg.direct_uses pdg n) @ (!Pdg.direct_dpds pdg n) in
    !Pdg.custom_related_nodes all
  in
  let nodes = all_related_nodes pdg y_at_11_nodes in
  pp_nodes "Test [all_related_nodes] y@11" nodes
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let () = Db.Main.extend main
