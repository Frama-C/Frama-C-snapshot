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

(* $Id: wserver.ml,v 1.3 2008/11/05 14:03:18 filliatr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

let sock_in = ref "wserver.sin"
let sock_out = ref "wserver.sou"
let noproc = ref false

let wserver_oc = set_binary_mode_out stdout true; ref stdout

let wprint fmt = Printf.fprintf !wserver_oc fmt
let wflush () = flush !wserver_oc

let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)

let hexa_val conf =
  match conf with
    '0'..'9' -> Char.code conf - Char.code '0'
  | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
  | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
  | _ -> 0

let decode s =
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
        '%' | '+' -> true
      | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          '%' when i + 2 < String.length s ->
            let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] in
            s1.[i1] <- Char.chr v; i + 3
        | '+' -> s1.[i1] <- ' '; succ i
        | x -> s1.[i1] <- x; succ i
      in
      copy_decode_in s1 i (succ i1)
    else s1
  in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else s
    else s
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = String.create len in
    strip_heading_and_trailing_spaces (copy_decode_in s1 0 0)
  else s

let special =
  function
    '\000'..'\031' | '\127'..'\255' | '<' | '>' | '\"' | '#' | '%' | '{' |
    '}' | '|' | '\\' | '^' | '~' | '[' | ']' | '`' | ';' | '/' | '?' | ':' |
    '@' | '=' | '&' ->
      true
  | _ -> false

let encode s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
        ' ' -> true
      | x -> if special x then true else need_code (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
          ' ' -> s1.[i1] <- '+'; succ i1
        | c ->
            if special c then
              begin
                s1.[i1] <- '%';
                s1.[i1 + 1] <- hexa_digit (Char.code c / 16);
                s1.[i1 + 2] <- hexa_digit (Char.code c mod 16);
                i1 + 3
              end
            else begin s1.[i1] <- c; succ i1 end
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s

let nl () = wprint "\013\010"

let http answer =
  let answer = if answer = "" then "200 OK" else answer in
  wprint "HTTP/1.0 %s" answer; nl ()

let print_exc exc =
  match exc with
    Unix.Unix_error (err, fun_name, arg) ->
      prerr_string "\"";
      prerr_string fun_name;
      prerr_string "\" failed";
      if String.length arg > 0 then
        begin prerr_string " on \""; prerr_string arg; prerr_string "\"" end;
      prerr_string ": ";
      prerr_endline (Unix.error_message err)
  | Out_of_memory -> prerr_string "Out of memory\n"
  | Match_failure (file, first_char, last_char) ->
      prerr_string "Pattern matching failed, file ";
      prerr_string file;
      prerr_string ", chars ";
      prerr_int first_char;
      prerr_char '-';
      prerr_int last_char;
      prerr_char '\n'
  | Assert_failure (file, first_char, last_char) ->
      prerr_string "Assertion failed, file ";
      prerr_string file;
      prerr_string ", chars ";
      prerr_int first_char;
      prerr_char '-';
      prerr_int last_char;
      prerr_char '\n'
  | x ->
      prerr_string "Uncaught exception: ";
      prerr_string (Obj.magic (Obj.field (Obj.field (Obj.repr x) 0) 0));
      if Obj.size (Obj.repr x) > 1 then
        begin
          prerr_char '(';
          for i = 1 to Obj.size (Obj.repr x) - 1 do
            if i > 1 then prerr_string ", ";
            let arg = Obj.field (Obj.repr x) i in
            if not (Obj.is_block arg) then prerr_int (Obj.magic arg : int)
            else if Obj.tag arg = 252 then
              begin
                prerr_char '\"';
                prerr_string (Obj.magic arg : string);
                prerr_char '\"'
              end
            else prerr_char '_'
          done;
          prerr_char ')'
        end;
      prerr_char '\n'

let print_err_exc exc = print_exc exc; flush stderr

let case_unsensitive_eq s1 s2 = String.lowercase s1 = String.lowercase s2

let rec extract_param name stop_char =
  function
    x :: l ->
      if String.length x >= String.length name &&
         case_unsensitive_eq (String.sub x 0 (String.length name)) name
      then
        let i =
          let rec loop i =
            if i = String.length x then i
            else if x.[i] = stop_char then i
            else loop (i + 1)
          in
          loop (String.length name)
        in
        String.sub x (String.length name) (i - String.length name)
      else extract_param name stop_char l
  | [] -> ""

let buff = ref (String.create 80)
let store len x =
  if len >= String.length !buff then
    buff := !buff ^ String.create (String.length !buff);
  !buff.[len] <- x;
  succ len
let get_buff len = String.sub !buff 0 len

let get_request strm =
  let rec loop len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\010' ->
        Stream.junk strm__;
        let s = strm__ in
        if len == 0 then [] else let str = get_buff len in str :: loop 0 s
    | Some '\013' -> Stream.junk strm__; loop len strm__
    | Some c -> Stream.junk strm__; loop (store len c) strm__
    | _ -> if len == 0 then [] else [get_buff len]
  in
  loop 0 strm

let timeout tmout spid _ =
  Unix.kill spid Sys.sigkill;
  http "";
  wprint "Content-type: text/html; charset=iso-8859-1";
  nl ();
  nl ();
  wprint "<head><title>Time out</title></head>\n";
  wprint "<body><h1>Time out</h1>\n";
  wprint "Computation time > %d second(s)\n" tmout;
  wprint "</body>\n";
  wflush ();
  exit 2

let get_request_and_content strm =
  let request = get_request strm in
  let content =
    match extract_param "content-length: " ' ' request with
      "" -> ""
    | x ->
        let str = String.create (int_of_string x) in
        for i = 0 to String.length str - 1 do
          str.[i] <-
            let (strm__ : _ Stream.t) = strm in
            match Stream.peek strm__ with
              Some x -> Stream.junk strm__; x
            | _ -> ' '
        done;
        str
  in
  request, content

let string_of_sockaddr =
  function
    Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
let sockaddr_of_string s = Unix.ADDR_UNIX s

let treat_connection tmout callback addr fd =
  ();
  let (request, script_name, contents__) =
    let (request, contents__) =
      let strm =
        let c = " " in
        Stream.from
          (fun _ -> if Unix.read fd c 0 1 = 1 then Some c.[0] else None)
      in
      get_request_and_content strm
    in
    let (script_name, contents__) =
      match extract_param "GET /" ' ' request with
        "" -> extract_param "POST /" ' ' request, contents__
      | str ->
          try
            let i = String.index str '?' in
            String.sub str 0 i,
            String.sub str (i + 1) (String.length str - i - 1)
          with
            Not_found -> str, ""
    in
    request, script_name, contents__
  in
  if script_name = "robots.txt" then
    begin
      http "";
      wprint "Content-type: text/plain";
      nl ();
      nl ();
      wprint "User-Agent: *";
      nl ();
      wprint "Disallow: /";
      nl ();
      wflush ();
      Printf.eprintf "Robot request\n";
      flush stderr
    end
  else
    begin
      begin try callback (addr, request) script_name contents__ with
        Unix.Unix_error (Unix.EPIPE, "write", _) -> ()
      | exc -> print_err_exc exc
      end;
      begin try wflush () with
        _ -> ()
      end;
      try flush stderr with
        _ -> ()
    end

let buff = String.create 1024

(* *)

let rec list_remove x =
  function
    [] -> failwith "list_remove"
  | y :: l -> if x = y then l else y :: list_remove x l

(* *)
(* *)
(* *)

(* *)

let wait_and_compact s =
  if Unix.select [s] [] [] 15.0 = ([], [], []) then
    begin
      Printf.eprintf "Compacting... ";
      flush stderr;
      Gc.compact ();
      Printf.eprintf "Ok\n";
      flush stderr
    end

let skip_possible_remaining_chars fd =
  let b = "..." in
  try
    let rec loop () =
      match Unix.select [fd] [] [] 5.0 with
        [_], [], [] ->
          let len = Unix.read fd b 0 (String.length b) in
          if len = String.length b then loop ()
      | _ -> ()
    in
    loop ()
  with
    Unix.Unix_error (Unix.ECONNRESET, _, _) -> ()

let accept_connection tmout max_clients callback s =
  wait_and_compact s;
  let (t, addr) = Unix.accept s in
  Unix.setsockopt t Unix.SO_KEEPALIVE true;
  let cleanup () =
    begin try Unix.shutdown t Unix.SHUTDOWN_SEND with
      _ -> ()
    end;
    begin try Unix.shutdown t Unix.SHUTDOWN_RECEIVE with
      _ -> ()
    end;
    try Unix.close t with
      _ -> ()
  in
  wserver_oc := Unix.out_channel_of_descr t;
  treat_connection tmout callback addr t;
  cleanup ()

let f addr_opt port tmout max_clients g =
  match None with
    Some s -> ()
  | None ->
      let addr =
        match addr_opt with
          Some addr ->
            begin try Unix.inet_addr_of_string addr with
              Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0)
            end
        | None -> Unix.inet_addr_any
      in
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt s Unix.SO_REUSEADDR true;
      Unix.bind s (Unix.ADDR_INET (addr, port));
      Unix.listen s 4;
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      let tm = Unix.localtime (Unix.time ()) in
      Printf.eprintf "Ready %4d-%02d-%02d %02d:%02d port"
        (1900 + tm.Unix.tm_year) (succ tm.Unix.tm_mon) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min;
      Printf.eprintf " %d" port;
      Printf.eprintf "...\n";
      flush stderr;
      while true do
        begin try accept_connection tmout max_clients g s with
          Unix.Unix_error (Unix.ECONNRESET, "accept", _) -> ()
        | Unix.Unix_error ((Unix.EBADF | Unix.ENOTSOCK), "accept", _) as x ->
            raise x
        | exc -> print_err_exc exc
        end;
        begin try wflush () with
          Sys_error _ -> ()
        end;
        begin try flush stdout with
          Sys_error _ -> ()
        end;
        flush stderr
      done
