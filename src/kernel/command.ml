(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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


(* -------------------------------------------------------------------------- *)
(* --- File Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

let filename parent child = Filename.concat parent child

let pp_to_file f pp =
  let cout = open_out f in
  let fout = Format.formatter_of_out_channel cout in
  try
    pp fout ;
    Format.pp_print_newline fout () ;
    Format.pp_print_flush fout () ;
    close_out cout
  with err ->
    Format.pp_print_newline fout () ;
    Format.pp_print_flush fout () ;
    close_out cout ;
    raise err

let pp_from_file fmt file =
  let cin = open_in file in
  try
    while true do
      !Db.progress () ;
      let line = input_line cin in
      Format.pp_print_string fmt line ;
      Format.pp_print_newline fmt () ;
    done
  with
    | End_of_file ->
        close_in cin
    | err ->
        close_in cin ;
        raise err

let rec bincopy buffer cin cout =
  let s = String.length buffer in
  let n = Pervasives.input cin buffer 0 s in
  if n > 0 then
    ( Pervasives.output cout buffer 0 n ; bincopy buffer cin cout )
  else
    ( Pervasives.output cout "\n" 0 1 ; flush cout )

let on_inc file job =
  let inc = open_in file in
  try job inc ; close_in inc
  with e -> close_in inc ; raise e

let on_out file job =
  let out = open_out file in
  try job out ; close_out out
  with e -> close_out out ; raise e

let copy src tgt =
  on_inc src
    (fun inc ->
       on_out tgt
         (fun out ->
            bincopy (String.create 2048) inc out))


(* -------------------------------------------------------------------------- *)
(* --- Process                                                            --- *)
(* -------------------------------------------------------------------------- *)

type process_result = Not_ready of (unit -> unit) | Result of Unix.process_status

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
  let last_result= ref(Not_ready (fun () -> Extlib.terminate_process pid))
  in
  (fun () ->
     match !last_result with
       | Result _ as r -> r
       | Not_ready _ as r ->
           let child_id,status =
             Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid
           in
           if child_id = 0 then r
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
  let to_terminate = ref None in
  let do_terminate () =
    begin match !to_terminate with
    | None -> ()
    | Some pid -> Extlib.terminate_process pid
    end;
    Extlib.safe_remove inf;
    Extlib.safe_remove outf;
    Extlib.safe_remove errf
  in
  at_exit do_terminate; (* small memory leak : pending list of ref None ... *)
  let pid = Unix.create_process cmd (Array.concat [[|cmd|];args])
    (Unix.descr_of_out_channel inc)
    (Unix.descr_of_out_channel outc)
    (Unix.descr_of_out_channel errc)
  in
  to_terminate:= Some pid;
  close_out inc; close_out outc; close_out errc;
    (*Format.printf "Generic run: %s " cmd;
      Array.iter (fun s -> Format.printf "%s " s) args;
      Format.printf "@.";*)
  let last_result= ref (Not_ready do_terminate) in
  let wait_flags = if async then [Unix.WNOHANG; Unix.WUNTRACED]
    else [Unix.WUNTRACED]
  in
  (fun () ->
    match !last_result with
    | Result _p as r ->
        (*Format.printf "Got result %d@."
          (match _p with Unix.WEXITED x -> x | _ -> 99);*)
      r
    | Not_ready _ as r ->
      let child_id,status = Unix.waitpid wait_flags pid in
      if child_id = 0 then (assert async;r)
      else (
        to_terminate := None;
          (*Format.printf "Got (%s) result after wait %d@."
            cmd (match status with Unix.WEXITED x -> x | _ -> 99);*)
        last_result := Result status;
        cleanup_and_fill stdout outf;
        cleanup_and_fill stderr errf;
        Extlib.safe_remove inf;
        !last_result))

let command_async ?stdout ?stderr cmd args =
  command_generic ~async:true ?stdout ?stderr cmd args

let command ?(timeout=0) ?stdout ?stderr cmd args =
  if !Config.is_gui || timeout > 0 then
    let f = command_generic ~async:true ?stdout ?stderr cmd args in
    let res = ref(Unix.WEXITED 99) in
    let elapsed = ref 0 in
    let running () =
      match f () with
        | Not_ready terminate ->
            begin
              try
                !Db.progress () ;
                if timeout > 0 && !elapsed > timeout then raise Db.Cancel ;
                true
              with Db.Cancel as e ->
                terminate ();
                raise e
            end
        | Result r ->
            res := r;
            false
    in while running () do Unix.sleep 1 done ; !res
  else
    let f = command_generic ~async:false ?stdout ?stderr cmd args in
    match f () with
    | Result r -> r
    | Not_ready _ -> assert false

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
