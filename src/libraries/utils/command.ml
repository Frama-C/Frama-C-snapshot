(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

let safe_close_out outc = try close_out outc with Sys_error _ -> ()
let safe_close_in inc = try close_in inc with Sys_error _ -> ()

(* -------------------------------------------------------------------------- *)
(* --- File Utilities                                                     --- *)
(* -------------------------------------------------------------------------- *)

let filename parent child = Filename.concat parent child

let pp_to_file f pp =
  let cout = open_out f in
  let fout = Format.formatter_of_out_channel cout in
  try
    pp fout ;
    Format.pp_print_flush fout () ;
    safe_close_out cout
  with err ->
    Format.pp_print_flush fout () ;
    safe_close_out cout ;
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
  let s = Bytes.length buffer in
  let n = Pervasives.input cin buffer 0 s in
  if n > 0 then
    ( Pervasives.output cout buffer 0 n ; bincopy buffer cin cout )
  else
    ( flush cout )

let on_inc file job =
  let inc = open_in file in
  let finally () = safe_close_in inc in
  Extlib.try_finally ~finally job inc

let on_out file job =
  let out = open_out file in
  let finally () = safe_close_out out in
  Extlib.try_finally ~finally job out

let copy src tgt =
  on_inc src
    (fun inc -> on_out tgt (fun out -> bincopy (Bytes.create 2048) inc out))

let read_file file job =
  let inc = open_in file in
  let finally () = safe_close_in inc in
  Extlib.try_finally ~finally job inc

let read_lines file job =
  read_file file
    (fun inc ->
       try
         while true do
           job (input_line inc) ;
         done
       with End_of_file -> ())

let write_file file job =
  assert (file <> "");
  let out = open_out file in
  let finally () = flush out; safe_close_out out in
  Extlib.try_finally ~finally job out

let print_file file job =
  write_file file
    (fun out ->
       let fmt = Format.formatter_of_out_channel out in
       let finally () = Format.pp_print_flush fmt () in
       Extlib.try_finally ~finally job fmt)

(* -------------------------------------------------------------------------- *)
(* --- Timing                                                             --- *)
(* -------------------------------------------------------------------------- *)

type timer = float ref
type 'a result = Result of 'a | Error of exn
let dt_max tm dt = match tm with Some r when dt > !r -> r := dt | _ -> ()
let dt_add tm dt = match tm with Some r -> r := !r +. dt | _ -> ()
let return = function Result x -> x | Error e -> raise e
let catch f x = try Result(f x) with e -> Error e
let time ?rmax ?radd job data =
  begin
    let t0 = Sys.time () in
    let re = catch job data in
    let t1 = Sys.time () in
    let dt = t1 -. t0 in
    dt_max rmax dt ;
    dt_add radd dt ;
    return re ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Process                                                            --- *)
(* -------------------------------------------------------------------------- *)

type process_result = Not_ready of (unit -> unit) | Result of Unix.process_status

let _pp_status fmt = function
  | Unix.WEXITED s -> Format.fprintf fmt "exit[%d]" s
  | Unix.WSIGNALED s -> Format.fprintf fmt "sig[%d]" s
  | Unix.WSTOPPED s -> Format.fprintf fmt "stop[%d]" s

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
  
let flush b f =
  match b with
  | None -> ()
  | Some b ->
      try read_lines f
            (fun line -> Buffer.add_string b line ; Buffer.add_char b '\n') ;
      with Sys_error _ -> ()

(*[LC] return the cancel function *)
let cancelable_at_exit job =
  let later = ref (Some job) in
  Extlib.safe_at_exit
    (fun () -> match !later with None -> () | Some job -> job ()) ;
  fun () -> later := None

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
  let delete () =
    begin
      Extlib.safe_remove inf;
      Extlib.safe_remove outf;
      Extlib.safe_remove errf;
    end in
  let deleted = cancelable_at_exit delete in
  let pid = Unix.create_process cmd (Array.append [|cmd|] args)
      (Unix.descr_of_out_channel inc)
      (Unix.descr_of_out_channel outc)
      (Unix.descr_of_out_channel errc)
  in
  let killed = cancelable_at_exit
      begin fun () ->
        Extlib.terminate_process pid ;
        Unix.(try ignore (waitpid [] pid) with Unix_error _ -> ()) ;
      end in
  safe_close_out inc;
  safe_close_out outc;
  safe_close_out errc;
  let kill () = Extlib.terminate_process pid in
  let last_result= ref (Not_ready kill) in
  let wait_flags =
    if async
    then [Unix.WNOHANG; Unix.WUNTRACED]
    else [Unix.WUNTRACED]
  in
  begin fun () ->
    match !last_result with
    | Result _p as r -> r
    | Not_ready _ as r ->
        let child_id,status = Unix.waitpid wait_flags pid in
        if child_id = 0 then (assert async;r)
        else
          begin
            let result = Result status in
            flush stdout outf ;
            flush stderr errf ;
            delete () ;
            deleted () ;
            killed () ;
            result
          end
  end

let command_async ?stdout ?stderr cmd args =
  command_generic ~async:true ?stdout ?stderr cmd args

let command ?(timeout=0) ?stdout ?stderr cmd args =
  if !Config.is_gui || timeout > 0 then
    let f = command_generic ~async:true ?stdout ?stderr cmd args in
    let res = ref(Unix.WEXITED 99) in
    let ftimeout = float_of_int timeout in
    let start = ref (Unix.gettimeofday ()) in
    let running () =
      match f () with
      | Not_ready terminate ->
          begin
            try
              !Db.progress () ;
              if timeout > 0 && Unix.gettimeofday () -. !start > ftimeout then
                raise Db.Cancel ;
              true
            with Db.Cancel as e ->
              terminate ();
              raise e
          end
      | Result r ->
          res := r;
          false
    in while running () do Extlib.usleep 100000 (* 0.1s *) done ; !res
  else
    let f = command_generic ~async:false ?stdout ?stderr cmd args in
    match f () with
    | Result r -> r
    | Not_ready _ -> assert false

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
