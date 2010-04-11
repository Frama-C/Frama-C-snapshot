(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

type process_result = Not_ready | Result of Unix.process_status
    
let full_command cmd args ~stdin ~stdout ~stderr =
  let pid = 
    Unix.create_process cmd (Array.concat [[|cmd|];args]) stdin stdout stderr 
  in
  let _,status = Unix.waitpid [Unix.WUNTRACED] pid in
  status

let full_command_async cmd args ~stdin ~stdout ~stderr =
  let pid = 
    Unix.create_process cmd (Array.concat [[|cmd|];args]) stdin stdout stderr 
  in
  let last_result= ref Not_ready in
  (fun () -> 
     match !last_result with 
       | Result _ as r -> r
       | Not_ready -> 
	   let child_id,status = 
	     Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid 
	   in
	   if child_id = 0 then Not_ready
	   else (last_result := Result status; !last_result))

let cleanup_and_fill b f = 
  match b with 
    | None -> Extlib.safe_remove f
    | Some b -> 
	try 
	  let cin = open_in_bin f in
	  (try 
	    while true do 
	      Buffer.add_string b (input_line cin);
	      Buffer.add_char b '\n'
	    done
	  with _ -> ());
	  close_in cin
	with _ -> 
	  Extlib.safe_remove f
	    
let command_generic ~async ?stdout ?stderr cmd args =
  let inf,inc = Filename.open_temp_file 
    ~mode:[Open_binary;Open_rdonly; Open_trunc; Open_creat; Open_nonblock ]
    "in_" ".tmp"
  in
  let outf,outc = Filename.open_temp_file 
    ~mode:[Open_binary;Open_wronly; Open_trunc; Open_creat]
    "out_" ".tmp"
  in
  let errf,errc = Filename.open_temp_file 
    ~mode:[Open_binary;Open_wronly; Open_trunc; Open_creat]
    "out_" ".tmp"
  in
  at_exit (fun () -> 
             Extlib.safe_remove inf;
             Extlib.safe_remove outf; 
             Extlib.safe_remove errf);
  let pid = Unix.create_process cmd (Array.concat [[|cmd|];args])
    (Unix.descr_of_out_channel inc) 
    (Unix.descr_of_out_channel outc)
    (Unix.descr_of_out_channel errc)
  in
  close_out inc; close_out outc; close_out errc;
  (*Format.printf "Generic run: %s " cmd;
    Array.iter (fun s -> Format.printf "%s " s) args;
    Format.printf "@.";*)
  let last_result= ref Not_ready in
  let wait_flags = if async then [Unix.WNOHANG; Unix.WUNTRACED]
  else [Unix.WUNTRACED]
  in
  (fun () -> 
     match !last_result with 
       | Result _p as r -> 
	   (*Format.printf "Got result %d@." 
	     (match _p with Unix.WEXITED x -> x | _ -> 99);*)
	   r
       | Not_ready -> 
	   let child_id,status = Unix.waitpid wait_flags pid in
	   if child_id = 0 then (assert async;Not_ready)
	   else (
	     (*Format.printf "Got (%s) result after wait %d@." 
	       cmd (match status with Unix.WEXITED x -> x | _ -> 99);*)
	     last_result := Result status; 
	     cleanup_and_fill stdout outf;
	     cleanup_and_fill stderr errf;
	     Extlib.safe_remove inf;
	     !last_result))

let command_async ?stdout ?stderr cmd args =
  command_generic ~async:true ?stdout ?stderr cmd args
    
let command ?stdout ?stderr cmd args =
  if !Config.is_gui then 
    let f = command_generic ~async:true ?stdout ?stderr cmd args in
    let res = ref(Unix.WEXITED 99) in
    while match f () with 
    | Not_ready -> true
    | Result r ->
        res := r;
        false
    do !Db.progress (); Unix.sleep 1 done;
    !res
  else
    let f = command_generic ~async:false ?stdout ?stderr cmd args in
    match f () with 
    | Result r -> r
    | Not_ready -> assert false

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
