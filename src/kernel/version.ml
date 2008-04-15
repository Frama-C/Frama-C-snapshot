(* This file is generated in Makefile.in. Do not modify. *)
let version = "Lithium-20081201"
let date = "Tue Dec 16 11:10:14 CET 2008"
let dataroot = try Sys.getenv "FRAMAC_SHARE" with Not_found -> "/usr/local/share/frama-c"
let static_plugins = [ "Occurrence"; "Metrics"; "Syntactic_callgraph"; "Value"; "From"; "Users"; "Constant_Propagation"; "Postdominators"; "Inout"; "Semantic_callgraph"; "Wp"; "Security"; "Impact"; "Jessie"; "Pdg"; "Scope"; "Sparecode"; "Slicing"; "Ltl_to_acsl" ]
let static_gui_plugins = [ "Occurrence_gui"; "Metrics_gui"; "Syntactic_callgraph_gui"; "Value_gui"; "From_gui"; "Wp_gui"; "Security_gui"; "Impact_gui"; "Scope_gui"; "Slicing_gui" ]
