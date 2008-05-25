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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: dp.ml,v 1.40 2008/02/05 12:10:50 marche Exp $ i*)

(* script to call automatic provers *)

open Format
open Calldp

let timeout = ref 10
let eclauses = ref 2000 (* E prover max nb of clauses *)
let debug = ref false
let batch = ref false
let timings = ref true (* print timings *)
let files = Queue.create ()

type smt_solver = Yices | CVC3 | Z3
let smt_solver = ref Yices
let set_smt_solver = function
  | "yices" -> smt_solver := Yices
  | "cvc3" -> smt_solver := CVC3
  | "z3" -> smt_solver := Z3
  | s -> eprintf "unknown SMT solver %s@." s; exit 1

let spec = 
  [ "-timeout", Arg.Int ((:=) timeout), "<int>  set the timeout (in seconds)";
    "-eclauses", Arg.Int ((:=) eclauses), 
    "<int>  set the max nb of clauses for the E prover";
    "-debug", Arg.Set debug, "set the debug flag";
    "-batch", Arg.Set batch, "run in batch mode";
    "-no-timings", Arg.Clear timings, "do not display timings";
    "-smt-solver", Arg.String set_smt_solver, "<solver>";
  ]

let usage = "usage: dp [options] files.{why,rv,znn,cvc,cvc.all,sx,sx.all,smt,smt.all}"
let () = Arg.parse spec (fun s -> Queue.push s files) usage 

let () = 
  Cvcl_split.debug := !debug; 
  Simplify_split.debug := !debug;
  Zenon_split.debug := !debug

(* stats *)

let nvalid = ref 0
let tvalid = ref 0.0
let tmaxvalid = ref 0.0

let ninvalid = ref 0
let tinvalid = ref 0.0
let tmaxinvalid = ref 0.0

let nunknown = ref 0
let tunknown = ref 0.0
let tmaxunknown = ref 0.0

let ntimeout = ref 0
let ttimeout = ref 0.0

let nfailure = ref 0
let tfailure = ref 0.0

let is_valid t = 
  if !batch then exit 0;
  printf "."; incr nvalid; 
  tvalid := !tvalid +. t;
  tmaxvalid := max !tmaxvalid t


let is_invalid t = 
  if !batch then exit 2;
  printf "*"; incr ninvalid; 
  tinvalid := !tinvalid +. t;
  tmaxinvalid := max !tmaxinvalid t

let is_unknown t = 
  if !batch then exit 3;
  printf "?"; incr nunknown; 
  tunknown := !tunknown +. t;
  tmaxunknown := max !tmaxunknown t

let is_timeout t = 
  if !batch then exit 4;
  printf "#"; incr ntimeout;
  ttimeout := !ttimeout +. t

let is_failure t = 
  if !batch then exit 5;
  printf "!"; incr nfailure;
  tfailure := !tfailure +. t 


let wrapper r = 
  begin match r with
    | Valid t -> is_valid t
    | Invalid(t,_) -> is_invalid t
    | CannotDecide (t,_) -> is_unknown t
    | Timeout t -> is_timeout t
    | ProverFailure(t,_) -> is_failure t
  end;
  flush stdout

let call_ergo f = 
  wrapper (Calldp.ergo ~debug:!debug ~timeout:!timeout ~filename:f ())
let call_cvcl f = 
  wrapper (Calldp.cvcl ~debug:!debug ~timeout:!timeout ~filename:f ())
let call_simplify f = 
  wrapper (Calldp.simplify ~debug:!debug ~timeout:!timeout ~filename:f ())
let call_yices f = 
  wrapper (Calldp.yices ~debug:!debug ~timeout:!timeout ~filename:f ())
let call_cvc3 f = 
  wrapper (Calldp.cvc3 ~debug:!debug ~timeout:!timeout ~filename:f ())
let call_z3 f = 
  wrapper (Calldp.z3 ~debug:!debug ~timeout:!timeout ~filename:f ())
let call_rvsat f = 
  wrapper (Calldp.rvsat ~debug:!debug ~timeout:!timeout ~filename:f ())
let call_zenon f = 
  wrapper (Calldp.zenon ~debug:!debug ~timeout:!timeout ~filename:f ())
let call_harvey f = 
  wrapper (Calldp.harvey ~debug:!debug ~timeout:!timeout ~filename:f ())

let call_smt_solver = match !smt_solver with
  | Yices -> call_yices
  | CVC3 -> call_cvc3
  | Z3 -> call_z3

let split f =
  if not !batch then printf "%-30s: " f;
  let oldv = !nvalid in
  let oldi = !ninvalid in
  let oldt = !ntimeout in
  let oldu = !nunknown in
  let oldf = !nfailure in
  if Filename.check_suffix f ".smt"  || Filename.check_suffix f ".smt.all" then
    begin
      Smtlib_split.iter call_smt_solver f 
    end 
  else
  if Filename.check_suffix f ".why" then
    begin
      Ergo_split.iter call_ergo f 
    end
  else 
  if Filename.check_suffix f ".cvc"  || Filename.check_suffix f ".cvc.all" then
    Cvcl_split.iter call_cvcl f 
  else 
  if Filename.check_suffix f ".sx" || 
     Filename.check_suffix f ".sx.all" ||
     Filename.check_suffix f ".simplify"
  then
    Simplify_split.iter call_simplify f 
  else 
  if Filename.check_suffix f ".znn" || Filename.check_suffix f ".znn.all" then
    Zenon_split.iter call_zenon f (* TODO: Zenon_split *)
  else 
  if Filename.check_suffix f ".rv" then
    begin
      Rv_split.iter  call_harvey f 
    end
  else 
    begin Arg.usage spec usage; exit 1 end;
  printf 
    " (%d/%d/%d/%d/%d)@." (!nvalid - oldv) (!ninvalid - oldi) (!nunknown - oldu) (!ntimeout - oldt) (!nfailure - oldf)
    
let print_time fmt f =
  if f < 60.0 then fprintf fmt "%.2f sec" f else 
    let t = int_of_float f in
    let m = t / 60 in
    let s = t mod 60 in
    if f < 3600.0 then fprintf fmt "%d m %02d sec" m s else 
      let h = m / 60 in
      let m = m mod 60 in
      fprintf fmt "%d h %02d m %02d sec" h m s  

let main () = 
  if Queue.is_empty files then begin Arg.usage spec usage; exit 1 end;
  let wctime0 = Unix.gettimeofday() in
  if not !batch then printf "(. = valid * = invalid ? = unknown # = timeout ! = failure)@."; 
  Queue.iter split files;
  let wctime = Unix.gettimeofday() -. wctime0 in
  let n = !nvalid + !ninvalid + !ntimeout + !nunknown + !nfailure in
  if n = 0 then exit 0;
  let pvalid = 100. *. float !nvalid /. float n in
  let pinvalid = 100. *. float !ninvalid /. float n in
  let ptimeout = 100. *. float !ntimeout /. float n in
  let punknown = 100. *. float !nunknown /. float n in
  let pfailure = 100. *. float !nfailure /. float n in
  printf 
"total   : %3d
valid   : %3d (%3.0f%%)
invalid : %3d (%3.0f%%)
unknown : %3d (%3.0f%%)
timeout : %3d (%3.0f%%)
failure : %3d (%3.0f%%)\n" n
    !nvalid pvalid !ninvalid pinvalid !nunknown punknown 
    !ntimeout ptimeout !nfailure pfailure;
  if !timings then printf
"total wallclock time : %a
total CPU time       : %a
valid VCs:
    average CPU time : %.2f
    max CPU time     : %.2f
invalid VCs:
    average CPU time : %.2f
    max CPU time     : %.2f
unknown VCs:
    average CPU time : %.2f
    max CPU time     : %.2f\n"
    print_time wctime
    print_time  (!tvalid +. !tinvalid +. !tunknown +. !ttimeout +. !tfailure)
    (!tvalid /. float !nvalid)
    !tmaxvalid
    (!tinvalid /. float !ninvalid)
    !tmaxinvalid
      (!tunknown /. float !nunknown)
    !tmaxunknown;


  try Sys.remove "out" with _ -> ()

let () = Printexc.catch main ()
