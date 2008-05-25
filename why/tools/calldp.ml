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

(*i $Id: calldp.ml,v 1.46 2008/02/12 13:41:29 marche Exp $ i*)

open Printf
open Options


type prover_result = 
  | Valid of float                         
  | Invalid of float * string option       
  | CannotDecide of float * string option  
  | Timeout of float                       
  | ProverFailure of float * string        




let remove_file ?(debug=false) f =
  if not debug then try Sys.remove f with _ -> ()

let file_contents f =
  let buf = Buffer.create 1024 in
  try
    let c = open_in f in
    begin try 
      while true do 
	let s = input_line c in Buffer.add_string buf s; 
	Buffer.add_char buf '\n'
      done;
      assert false
    with End_of_file ->
      close_in c;
      Buffer.contents buf
    end
  with _ -> 
    sprintf "(cannot open %s)" f

let timed_sys_command ~debug timeout cmd =
  let t0 = Unix.times () in
  if debug then Format.eprintf "command line: %s@." cmd;
  let out = Filename.temp_file "out" "" in
  let cmd = sprintf "cpulimit %d %s > %s 2>&1" timeout cmd out in
  let ret = Sys.command cmd in
  let t1 = Unix.times () in
  let cpu_time = t1.Unix.tms_cutime -. t0.Unix.tms_cutime in
  if debug then Format.eprintf "Output file %s:@.%s@." out (file_contents out);
  (cpu_time,ret,out)

let error c t cmd =
  if c = 152 then Timeout t 
  else ProverFailure (t,"command failed: " ^ cmd) 

let cvcl ?(debug=false) ?(timeout=10) ~filename:f () =
  let cmd = sprintf "cvcl < %s" f in
  let t,c,out = timed_sys_command debug timeout cmd in
  if c <> 0 then error c t cmd
  else if Sys.command (sprintf "grep -q -w -i Error %s" out) = 0 then
    ProverFailure(t,"command failed: " ^ cmd ^ "\n" ^ file_contents out)
  else
    let r=
      let c = Sys.command (sprintf "grep -q -w Valid %s" out) in
      if c = 0 then Valid t
      else 
	let c = Sys.command (sprintf "grep -q -w Unknown %s" out)  in
	if c = 0 then 
	  CannotDecide(t,Some (file_contents out)) 
	else
	  Invalid (t, Some (file_contents out))
    in
    remove_file ~debug out;
    r

let simplify ?(debug=false) ?(timeout=10) ~filename:f () =
  let cmd = sprintf "Simplify %s" f in
  let t,c,out = timed_sys_command ~debug timeout cmd in
  if c <> 0 then error c t cmd
  else 
    let r =
      if Sys.command (sprintf "grep -q -w Valid %s" out) = 0 then
	Valid t
      else
	if Sys.command (sprintf "grep -q -w Invalid %s" out) = 0 then
	  CannotDecide (t,Some (file_contents out)) 
	else
	  ProverFailure(t,"command failed: " ^ cmd ^ "\n" ^ file_contents out)
    in
    remove_file ~debug out;
    r


(**
   Graph is an interface which aims at recursively calling 
   the hypotheses_filtering module if needed.
   
   @param timeout is the global timeout for the proof
   @param f : is the name of the input file which contains the proof obligation

   For the moment, the proof obligation is stored as a why file. 
   
   The first part try to check whether the formula f is valid by 
   discharging it into simplify in a timeout set to timeout/4.
   The function exits when the result is valid, unknown or not valid.

   Otherwise, the function enters in a second step.
   The hypotheses_filtering module is  
   called with with a depth that increases, provided 
   the result returned by the prover is not_valid or unknown.
   Face to a timeout result, the prover is called again with the same PO but 
   with a longer timeout.
**)
let graph  ?(debug=false) ?(timeout=10) ~filename:f () =
  let pruning_hyp = 3 in 
  let last_dot_index = String.rindex f '.' in 
  let f_for_simplify = (String.sub f  0 last_dot_index) ^ "_why.sx" in 
  let cmd = sprintf "why --simplify --no-prelude %s " f in
  let t'= 
    (float_of_int timeout) /. (float_of_int (pruning_hyp +1)) in
  let t'',c,out = timed_sys_command ~debug (int_of_float t') cmd in
  let cmd = sprintf "Simplify %s"  f_for_simplify in
  let t'',c,out = timed_sys_command ~debug (int_of_float (t' -. t'')) cmd in
  let result_sort t'' out  = 
    if Sys.command (sprintf "grep -q -w Valid %s" out) = 0 then
      Valid t''
    else
      if Sys.command (sprintf "grep -q -w Invalid %s" out) = 0 then
	CannotDecide (t'',Some (file_contents out))
      else
	ProverFailure
	  (t'',"command failed: " ^ cmd ^ "\n" ^ file_contents out) in
  if c == 0 then 
    let r = result_sort t'' out in
    remove_file ~debug out;
    r
  else 
    let t = ref 0.0 in 
    let c = ref 0 in 
    let vb = ref 0 in
    let pb = ref 1 in
    let explicitRes = ref true in
    let r = ref (Valid 0.0) in 
    while ( !c == 0 && !explicitRes  &&  !t < float_of_int timeout) 
    
    (**** UPDATE THIS WITH A LOOP OVER PB AS DONE IN THE ARTICLE CouchotHubert07**)

    do
      (* compute the new proof obligation *)
      let cmd = sprintf "why --simplify --no-prelude --prune-hyp %d %d %s " !pb !vb f  in
      let t'',c',out = timed_sys_command ~debug (int_of_float t') cmd in
    
      let cmd = sprintf "Simplify %s"  f_for_simplify in
      let t'',c',out = timed_sys_command ~debug (int_of_float (t' -. t'')) cmd in

      t :=  !t +. t'';
      c := c';
      r := result_sort t'' out ;
      vb := !vb+1 ;
      explicitRes := match !r with 
	  Valid _ | Timeout _ | ProverFailure _  -> false 
	| Invalid _ | CannotDecide  _ ->   true ;
	 
    done;
    
    let res  = 
      if !t >= float_of_int timeout then 
	error !c (float_of_int timeout) cmd
      else 
	if !c != 0 then 
	  error !c (float_of_int timeout) cmd
	else
	  !r in
    res
          
	  
      
    


let rvsat ?(debug=false) ?(timeout=10) ~filename:f () =
  (*let cmd = sprintf "rv-sat %s" f in*)
  let cmd = sprintf "rv-sat %s" f in
  let t,c,out = timed_sys_command ~debug timeout cmd in
  if c <> 0 then error c t cmd
  else 
    let r =
      if Sys.command (sprintf "grep -q -w unsat %s" out) = 0 then
	Valid t
      else	
	if Sys.command (sprintf "grep -q -w sat %s" out) = 0 then
	  Invalid (t, None)
	else
	  ProverFailure(t,"command failed: " ^ cmd)
    in
    (*remove_file ~debug out;*)
    r

let yices ?(debug=false) ?(timeout=30) ~filename:f () =
  let cmd = sprintf "yices  -pc 0 -smt < %s" f in
  let t,c,out = timed_sys_command ~debug timeout cmd in
  if c <> 0 then 
    if c==1 && 
      Sys.command (sprintf "grep -q -w 'feature not supported' %s" out) = 0 then
	ProverFailure(t,"command failed: " ^ cmd)
    else	
      error c t cmd
  else 
    let r = 
      if Sys.command (sprintf "grep -q -w unsat %s" out) = 0 then
	Valid t
      else
	if Sys.command (sprintf "grep -q -w unknown %s" out) = 0 then
	CannotDecide (t, None)
      else
	ProverFailure(t,"command failed: " ^ cmd)
    in
    remove_file ~debug out;
    r

let cvc3 ?(debug=false) ?(timeout=30) ~filename:f () =
  let cmd = 
    sprintf "cvc3 +arith-new +quant-polarity -quant-new -lang smt < %s" f 
  in
  let t,c,out = timed_sys_command ~debug timeout cmd in
  if c <> 0 then 
    if c==1 && 
      Sys.command (sprintf "grep -q -w 'feature not supported' %s" out) = 0 then
	ProverFailure(t,"command failed: " ^ cmd)
    else	
      error c t cmd
  else 
    let r = 
      if Sys.command (sprintf "grep -q -w unsat %s" out) = 0 then
	Valid t
      else if Sys.command (sprintf "grep -q -w sat %s" out) = 0 then
	CannotDecide (t, None)
      else if Sys.command (sprintf "grep -q -w unknown %s" out) = 0 then
	CannotDecide (t, None)
      else
	ProverFailure(t,"command failed: " ^ cmd)
    in
    remove_file ~debug out;
    r

let z3 ?(debug=false) ?(timeout=30) ~filename:f () =
  let cmd = sprintf "z3 -smt %s" f in
  let t,c,out = timed_sys_command ~debug timeout cmd in
  if c <> 0 then 
    if c==1 && 
      Sys.command (sprintf "grep -q -w 'feature not supported' %s" out) = 0 then
	ProverFailure(t,"command failed: " ^ cmd)
    else	
      error c t cmd
  else 
    let r = 
      if Sys.command (sprintf "grep -q -w unsat %s" out) = 0 then
	Valid t
      else
	if Sys.command (sprintf "grep -q -w unknown %s" out) = 0 then
	CannotDecide (t, None)
      else
	ProverFailure(t,"command failed: " ^ cmd)
    in
    remove_file ~debug out;
    r

let harvey ?(debug=false) ?(timeout=10) ~filename:f () =
  let cmd = sprintf "rvc %s" f in
  let t,c,out = timed_sys_command ~debug timeout cmd in
  if c <> 0 then (error c t cmd)
  else begin
    let f = Filename.chop_suffix f ".rv" in
    let fi = f ^ "-0"  ^ ".baf" in
    let cmd = sprintf "rv   %s" fi in
    let t,c,out = timed_sys_command ~debug timeout cmd in
    if c <> 0 then (error c t cmd)
    else
      let r =
	if Sys.command 
	  (sprintf "grep  -q -w \"is valid\" %s " out) = 0 then
	    Valid t
	else
	  if Sys.command 
	    (sprintf "grep  -q -w \"cannot be decided\" %s " out) = 0 
	  then
	    CannotDecide (t, None)
	  else
	    ProverFailure(t,"command failed: " ^ cmd)
      in
      remove_file ~debug out;
      r
  end
 


let ergo ?(debug=false) ?(timeout=10) ~filename:f () =
  let cmd = sprintf "ergo %s" f in
  let t,c,out = timed_sys_command ~debug timeout cmd in
  if c <> 0 then error c t cmd
  else 
    let r =
      if Sys.command (sprintf "grep -q -w Valid %s" out) = 0 then
	Valid t
      else if Sys.command (sprintf "grep -q -w \"I don't know\" %s" out) = 0
      then
	CannotDecide (t, None)
      else if Sys.command (sprintf "grep -q -w \"Invalid\" %s" out) = 0
      then
	(* ergo 0.6 never really say 'invalid' *)
	CannotDecide (t,None)
      else
	ProverFailure(t,"command failed: " ^ cmd)
    in
    remove_file ~debug out;
    r
    



 
	      
	

let zenon ?(debug=false) ?(timeout=10) ~filename:f () =
  let cmd = sprintf "zenon %s" f in
  let t,c,out = timed_sys_command ~debug timeout cmd in
  if c <> 0 then error c t cmd
  else 
    let r =
      if Sys.command (sprintf "grep -q PROOF-FOUND %s" out) = 0 then
	Valid t
      else
	ProverFailure(t,"command failed: " ^ cmd)
    in
    remove_file ~debug out;
    r
