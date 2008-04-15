(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: toolstat.ml,v 1.13 2008/11/25 12:44:22 moy Exp $ i*)

(* Statistics on automatic provers results *)

open Format
open Toolstat_types

let spec = [
  "-d",
  Arg.Set Toolstat_lex.debug,
  "  Set debug mode";
  "-d2",
  Arg.Set Toolstat_lex.debug_more,
  "  Set more debug mode";
  "-no-parse",
  Arg.Set Toolstat_lex.no_parsing,
  "  Only lex file";
]
let msg = "tool-stat file"
let records = ref []
let annots = ref []

let rec explain_exception fmt = function
  | Parsing.Parse_error -> 
      fprintf fmt "Syntax error"
  | Stream.Error s -> 
      fprintf fmt "Syntax error: %s" s
  | Loc.Located (loc, e) ->
      fprintf fmt "%a%a" Loc.report_position loc explain_exception e
  | e ->
      fprintf fmt "Anomaly: %s" (Printexc.to_string e); raise e

let parse_file f =
  try
    let c = open_in f in
    let lb = Lexing.from_channel c in
    lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
    let ann,recs = Toolstat_lex.parse lb in
    annots := ann;
    records := recs;
    close_in c
  with e -> 
    explain_exception err_formatter e;
    pp_print_newline err_formatter ();
    exit 1

let default_detail = ([],[],[],[],[])
let default_time = (0,0,0.)
let default_annot = (0,0,0,0)
    
let add_times (h1,m1,s1) (h2,m2,s2) =
  let h3 = h1 + h2 and m3 = m1 + m2 and s3 = s1 +. s2 in
  let carry = (int_of_float (floor s3)) / 60 in
  let m3 = m3 + carry and s3 = s3 -. (float_of_int (60 * carry)) in
  let carry = m3 / 60 in
  let h3 = h3 + carry and m3 = m3 - (60 * carry) in
  (h3,m3,s3)

let hadd single combine h k v =
  try
    let ls = Hashtbl.find h k in
    Hashtbl.replace h k (combine v ls)
  with Not_found ->
    Hashtbl.replace h k (single v)

let intadd h k v = hadd (fun x -> x) ( + ) h k v
let listadd h k v = hadd (fun x -> [x]) (fun x l -> x::l) h k v
let timeadd = hadd (fun x -> x) add_times

let hfind default h k =
  try Hashtbl.find h k with Not_found -> default

let valid_summary (n1,n2,n3,n4,n5) = n1
let notvalid_summary (n1,n2,n3,n4,n5) = n2 + n3 + n4 + n5
let make_summary (s1,s2,s3,s4,s5) =
  List.length s1, List.length s2, List.length s3, List.length s4, List.length s5

let valid_detail (s1,s2,s3,s4,s5) = s1
let notvalid_detail (s1,s2,s3,s4,s5) = s2 @ s3 @ s4 @ s5
let combine_details (s1,s2,s3,s4,s5) (t1,t2,t3,t4,t5) =
  let unique = 
    let table = Hashtbl.create 5 in
    function l ->
      List.fold_left 
	(fun acc e ->
	   if Hashtbl.mem table e then acc else 
	     (Hashtbl.replace table e (); e :: acc)
	) [] l
  in
  let u1 = unique (s1 @ t1) in (* Start with valid *)
  let u2 = unique (s2 @ t2) in (* Order of invalid ones unimportant *)
  let u3 = unique (s3 @ t3) in
  let u4 = unique (s4 @ t4) in
  let u5 = unique (s5 @ t5) in
  (u1, u2, u3, u4, u5)

let print_time ?(sec=false) ?(min=false) ?(hour=false) fmt (h,m,s) =
  if sec || min || hour then
    let s = 3600 * h + 60 * m + (int_of_float (floor s)) in
    if sec then 
      fprintf fmt "%d s" s
    else if min then
      fprintf fmt "%.2f m" (float_of_int s /. 60.)
    else  
      fprintf fmt "%.2f h" (float_of_int s /. 3600.)
  else if h = 0 && m = 0 && s = 0. then 
    fprintf fmt "0. s"
  else
    fprintf fmt "%a%a%a"
      (fun fmt h -> if h = 0 then () else fprintf fmt "%d h " h) h
      (fun fmt m -> if m = 0 then () else fprintf fmt "%d m " m) m
      (fun fmt s -> if s = 0. then () else fprintf fmt "%.2f s " s) s

let compare_time (h1,m1,s1) (h2,m2,s2) =
  let c = compare h1 h2 in
  if c <> 0 then c else
    let c = compare m1 m2 in
    if c <> 0 then c else
      compare s1 s2    

let () = 
  Arg.parse spec parse_file msg;
  let records : record list = !records in
  let annots : annotation list = !annots in

  let provers : (prover, unit) Hashtbl.t = Hashtbl.create 5 in
  let tests : (test, unit) Hashtbl.t = Hashtbl.create 5 in
  let error_tests : (test, unit) Hashtbl.t = Hashtbl.create 5 in
  let vc : (project * test * int, unit) Hashtbl.t = Hashtbl.create 5 in
  let vc_count : (project * test * int, int) Hashtbl.t = Hashtbl.create 5 in
  List.iter 
    (fun (completed,project,prover,test,summary,detail,time) ->
       (* useful to keep track of tests with 0 VC and tests in error *)
       Hashtbl.replace tests test ();
       if completed then
	 (Hashtbl.replace provers prover ();
	  List.iter
	    (fun i -> 
	       Hashtbl.replace vc (project,test,i) ();
	       intadd vc_count (project,test,i) 1
	    ) (valid_detail detail);
	  List.iter
	    (fun i -> 
	       Hashtbl.replace vc (project,test,i) ()
	    ) (notvalid_detail detail))
       else
	 (* At least one prover was in error on this test *)
	 Hashtbl.replace error_tests test ()
    ) records;

  (* Fill [prover_variants] with pairs of a variant and the original prover,
     for those provers that have variants *)
  let prover_variants : (prover, prover) Hashtbl.t = Hashtbl.create 5 in
  let tests_variants : ((project * prover * test), record list) Hashtbl.t 
      = Hashtbl.create 5 
  in
  let combined_prover prover = prover ^ " (all)" in
  Hashtbl.iter 
    (fun prover1 () ->
       Hashtbl.iter 
	 (fun prover2 () ->
	    if String.length prover1 > String.length prover2 
	      && String.sub prover1 0 (String.length prover2) = prover2 then
		Hashtbl.replace prover_variants prover1 prover2
	 ) provers
    ) provers;
  Hashtbl.iter
    (fun _variant_prover prover ->
       Hashtbl.replace prover_variants prover prover
    ) prover_variants;
  Hashtbl.iter
    (fun variant_prover prover ->
       Hashtbl.replace prover_variants variant_prover (combined_prover prover);
       Hashtbl.replace provers (combined_prover prover) ()
    ) prover_variants;
  List.iter
      (fun (completed,project,prover,test,summary,detail,time) ->
	 try 
	   let combined_prover = Hashtbl.find prover_variants prover in
	   listadd tests_variants (project,combined_prover,test)
	     (completed,project,combined_prover,test,summary,detail,time)  
	 with Not_found -> ()
      ) records;
  let records =
    Hashtbl.fold 
      (fun (project,prover,test) records acc ->
	 let completed,detail,time =
	   List.fold_left 
	     (fun (completed_acc,detail_acc,time_acc) 
		(completed,_project,_prover,_test,_summary,detail,time) ->
		  completed_acc || completed,
		  combine_details detail_acc detail,
		  add_times time_acc time
	     ) (false,default_detail,default_time) records
	 in
	 let summary = make_summary detail in
	 (completed,project,prover,test,summary,detail,time) :: acc
      ) tests_variants records
  in

  printf "@.Best individual provers:@.";
  let provers_valid : (prover, int) Hashtbl.t = Hashtbl.create 17 in
  let provers_notvalid : (prover, int) Hashtbl.t = Hashtbl.create 17 in
  List.iter (fun (completed,project,prover,test,summary,detail,time) ->
	       if completed then
		 (intadd provers_valid prover (valid_summary summary);
		  intadd provers_notvalid prover (notvalid_summary summary))
	    ) records;
  let provers_data = 
    Hashtbl.fold (fun p () acc ->
		    (p, 
		     hfind 0 provers_valid p, 
		     hfind 0 provers_notvalid p)
		    :: acc
		 ) provers [] 
  in
  let provers_ranking =
    List.sort (fun (p1,v1,n1) (p2,v2,n2) -> compare v2 v1) provers_data
  in
  ignore (List.fold_left 
	    (fun i (p,v,n) ->
	       printf "%d: %s   \t%d valid \t%d not valid \t%d%% proved@." 
		 i p v n 
		 (if v + i <> 0 then v * 100 / (v + n) else 0);
	       i+1
	    ) 1 provers_ranking);
  
  printf "@.Best combination provers:@.";
  let provers_ahead = Hashtbl.create 17 in
  let provers_behind = Hashtbl.create 17 in
  List.iter (fun (completed,project,prover,test,summary,detail,time) ->
	       if completed then
		 (List.iter
		    (fun i ->
		       assert (Hashtbl.mem vc_count (project,test,i));
		       if Hashtbl.find vc_count (project,test,i) = 1 then
			 intadd provers_ahead prover 1
		    ) (valid_detail detail);
		  List.iter
		    (fun i ->
		    if hfind 0 vc_count (project,test,i) > 0 then
		      intadd provers_behind prover 1
		    ) (notvalid_detail detail))
	    ) records;
  let provers_data =
    Hashtbl.fold (fun p () acc ->
		    (p,
		     hfind 0 provers_ahead p,
		     hfind 0 provers_behind p)
		    :: acc
		 ) provers []
  in
  let provers_ranking =
    List.sort (fun (p1,a1,b1) (p2,a2,b2) ->
		 let c = compare a2 a1 in
		 if c = 0 then compare b1 b2 else c)
      provers_data
  in
  ignore (List.fold_left
	    (fun i (p,a,b) ->
	       printf "%d: %s   \t%d alone \t%d by others@." i p a b;
	       i+1
	    ) 1 provers_ranking);

  printf "@.Quickest provers:@.";
  let provers_time : (prover, time) Hashtbl.t = Hashtbl.create 17 in
  List.iter (fun (completed,project,prover,test,summary,detail,time) ->
	       if completed then
		 timeadd provers_time prover time
	    ) records;
  let provers_data = 
    Hashtbl.fold (fun p () acc ->
		    (p, 
		     hfind (0,0,0.) provers_time p)
		    :: acc
		 ) provers [] 
  in
  let provers_ranking =
    List.sort (fun (p1,t1) (p2,t2) -> compare_time t1 t2) provers_data
  in
  ignore (List.fold_left 
	    (fun i (p,t) ->
	       printf "%d: %s   \t%a \t%a \t%a \t%a@." 
		 i p (fun fmt -> print_time fmt) t 
		 (fun fmt -> print_time ~sec:true fmt) t
		 (fun fmt -> print_time ~min:true fmt) t
		 (fun fmt -> print_time ~hour:true fmt) t;
	       i+1
	    ) 1 provers_ranking);

  let pre,post,inv,bwd =
    List.fold_left 
      (fun (pre1,post1,inv1,bwd1) (_project,(pre2,post2,inv2,bwd2)) ->
	 (pre1+pre2,post1+post2,inv1+inv2,bwd1+bwd2)
      ) default_annot annots 
  in
  printf "@.Number of relations in annotations generated:@.";
  printf "%d in preconditions@." pre;
  printf "%d in postconditions@." post;
  printf "%d in loop invariants@." inv;
  printf "%d in backward loop invariants@." bwd;
  
  let tests_notproved = Hashtbl.create 17 in
  let projects_notproved = Hashtbl.create 17 in
  Hashtbl.iter (fun (project,test,i) () ->
		  if hfind 0 vc_count (project,test,i) = 0 then
		    (intadd tests_notproved test 1;
		     match project with 
		       | None -> ()
		       | Some project -> intadd projects_notproved project 1)
	       ) vc;
  printf "@.Projects not proved: %d@." (Hashtbl.length projects_notproved);
  Hashtbl.iter (fun project n ->
		  printf "%s \t%d not proved@." project n
	       ) projects_notproved;
  printf "@.Tests not proved: %d@." (Hashtbl.length tests_notproved);
  Hashtbl.iter (fun test n ->
		  printf "%s \t%d not proved@." test n
	       ) tests_notproved;

  let numtests_notproved = Hashtbl.create 17 in
  Hashtbl.iter (fun project n ->
		  intadd numtests_notproved n 1
	       ) projects_notproved;
  printf "@.Num tests not proved by project:@.";
  let numtests_notproved = 
    Hashtbl.fold (fun n numproj acc -> (n,numproj) :: acc) numtests_notproved []
  in
  let numtests_notproved =
    List.sort (fun (n1,_) (n2,_) -> Pervasives.compare n1 n2) numtests_notproved
  in
  List.iter (fun (n,numproj) ->
	       printf "%d not proved for \t%d projects@." n numproj
	    ) numtests_notproved;

  let tests_proved = Hashtbl.create 17 in
  let projects_proved = Hashtbl.create 17 in
  Hashtbl.iter (fun (project,test,i) () ->
		  if hfind 0 tests_notproved test = 0 then
		    intadd tests_proved test 1;
		  match project with 
		    | None -> ()
		    | Some project ->
 			if hfind 0 projects_notproved project = 0 then
			  intadd projects_proved project 1
	       ) vc;
  printf "@.Projects proved: %d@." (Hashtbl.length projects_proved);
  Hashtbl.iter (fun project n ->
		  printf "%s \t%d proved@." project n
	       ) projects_proved;
  printf "@.Tests proved: %d@." (Hashtbl.length tests_proved);
  Hashtbl.iter (fun test n ->
		  printf "%s \t%d proved@." test n
	       ) tests_proved;
		  
  let tests_in_error = Hashtbl.create 17 in
  let tests_no_vc = Hashtbl.create 17 in
  Hashtbl.iter (fun test () ->
		  if not (Hashtbl.mem tests_notproved test)
		    && not (Hashtbl.mem tests_proved test) 
		  then
		    if Hashtbl.mem error_tests test then
		      Hashtbl.replace tests_in_error test ()
		    else
		      Hashtbl.replace tests_no_vc test ()		      
	       ) tests;
  printf "@.Tests in error: %d@." (Hashtbl.length tests_in_error);
  Hashtbl.iter (fun test n ->
		  printf "%s@." test
	       ) tests_in_error;
  printf "@.Tests with no VC: %d@." (Hashtbl.length tests_no_vc);
  Hashtbl.iter (fun test n ->
		  printf "%s@." test
	       ) tests_no_vc;
		  
  printf "@."

(*
Local Variables:
compile-command: "LC_ALL=C make -C .."
End:
*)
