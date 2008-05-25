open Db;;
open Cil_types;;

let pp_nodes msg nodes = Cil.log "%s\n" msg ;
  List.iter (fun n -> Cil.log "%a@." (!Pdg.pretty_node false) n) nodes;;

let f = Globals.Functions.find_by_name "f";;
let pdg = !Pdg.get f;;

(* Uncomment to retrieve sid *)
(*Cmdline.Debug.set 1;;
Format.eprintf "@[%a@]@." !Ast_printer.d_global (Kernel_function.get_global f);;
*)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
let stmt_1 = fst (Kernel_function.find_from_sid 0);; (* y = 0 *)
let node = !Pdg.find_stmt_node pdg stmt_1;;
let nodes = !Pdg.all_uses pdg [node];;
let () = pp_nodes "Test [all_uses] stmt1" nodes;;

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
exception Find of varinfo;;
let y =
  try
    Globals.Vars.iter (fun v _ -> if v.vname = "y" then raise (Find v));
    assert false
  with Find v ->
    v;;

let y_zone = Locations.valid_enumerate_bits (Locations.loc_of_varinfo y);;

let y_at_11_nodes, undef = (* y=5 *)
  !Pdg.find_location_nodes_at_stmt
    pdg (fst (Kernel_function.find_from_sid 9)) ~before:false y_zone;;

assert (Locations.Zone.equal undef Locations.Zone.bottom);;

let () = pp_nodes "Test [find_location_nodes_at_stmt] y@11" y_at_11_nodes;;
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
let nodes = !Pdg.all_dpds pdg y_at_11_nodes;;
let () = pp_nodes "Test [all_dpds] y@11" nodes;;
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
let nodes = !Pdg.all_uses pdg y_at_11_nodes;;
let () = pp_nodes "Test [all_uses] y@11" nodes;;
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
let all_related_nodes pdg =
  let all n = (!Pdg.direct_uses pdg n) @ (!Pdg.direct_dpds pdg n) in
  !Pdg.custom_related_nodes all;;
let nodes = all_related_nodes pdg y_at_11_nodes;;
let () = pp_nodes "Test [all_related_nodes] y@11" nodes;;
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
