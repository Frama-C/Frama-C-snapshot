(*****************************************************************************)
(*                                                                           *)
(*  This file was originally part of Objective Caml                          *)
(*                                                                           *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt               *)
(*                                                                           *)
(*  Copyright (C) 1996 INRIA                                                 *)
(*    INRIA (Institut National de Recherche en Informatique et en            *)
(*           Automatique)                                                    *)
(*                                                                           *)
(*  All rights reserved.                                                     *)
(*                                                                           *)
(*  This file is distributed under the terms of the GNU Library General      *)
(*  Public License version 2, with the special exception on linking          *)
(*  described below. See the GNU Library General Public License version      *)
(*  2 for more details (enclosed in the file licenses/LGPLv2).               *)
(*                                                                           *)
(*  As a special exception to the GNU Library General Public License,        *)
(*  you may link, statically or dynamically, a "work that uses the Library"  *)
(*  with a publicly distributed version of the Library to                    *)
(*  produce an executable file containing portions of the Library, and       *)
(*  distribute that executable file under terms of your choice, without      *)
(*  any of the additional requirements listed in clause 6 of the GNU         *)
(*  Library General Public License.                                          *)
(*  By "a publicly distributed version of the Library",                      *)
(*  we mean either the unmodified Library as                                 *)
(*  distributed by INRIA, or a modified version of the Library that is       *)
(*  distributed under the conditions defined in clause 2 of the GNU          *)
(*  Library General Public License.  This exception does not however         *)
(*  invalidate any other reasons why the executable file might be            *)
(*  covered by the GNU Library General Public License.                       *)
(*                                                                           *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux           *)
(*                        énergies alternatives).                            *)
(*                                                                           *)
(*****************************************************************************)

(* Extensible buffers *)

type t =
 {mutable buffer : bytes;
  mutable position : int;
  mutable length : int;
  initial_buffer : bytes}

let create n =
 let n = if n < 1 then 1 else n in
 let n = if n > Sys.max_string_length then Sys.max_string_length else n in
 let s = Bytes.create n in
 {buffer = s; position = 0; length = n; initial_buffer = s}

let contents b = Bytes.sub_string b.buffer 0 b.position
let to_bytes b = Bytes.sub b.buffer 0 b.position

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Buffer.sub"
  else Bytes.sub_string b.buffer ofs len
;;

let sub_bytes b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Buffer.sub_bytes"
  else Bytes.sub b.buffer ofs len
;;

let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
  then invalid_arg "Buffer.blit"
  else
    Bytes.unsafe_blit src.buffer srcoff dst dstoff len
;;

(* [calc_size cur_len req_len] computes the new size for a buffer having
   [cur_len] bytes, to ensure that it can contain at least [req_len] bytes.
   Used by [add_*] and [blit_*] functions.
   Raise [Failure] if the new size is too large. *)
let calc_size cur_len req_len =
  let new_len = ref cur_len in
  while req_len > !new_len do new_len := 2 * !new_len done;
  if !new_len > Sys.max_string_length then begin
    if req_len <= Sys.max_string_length
    then new_len := Sys.max_string_length
    else failwith "Buffer.add: cannot grow buffer"
  end;
  !new_len

(* [resize_aux b len] resizes buffer [b] to ensure it may contain at least
   [len] bytes. *)
let resize_aux b len =
  let new_len = calc_size b.length len in
  let new_buffer = Bytes.create new_len in
  b.buffer <- new_buffer;
  b.length <- new_len

let resize b len =
  let old_buffer = b.buffer in
  resize_aux b len;
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit old_buffer 0 b.buffer 0 b.position

let blit_substring_aux src srclen srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > srclen - len
     || dstoff < 0 || dstoff > dst.position
  then invalid_arg "Buffer.blit_*/add_*"
  else
    let new_pos = dstoff + len in
    let new_len = calc_size dst.length new_pos in
    if new_len > dst.length then begin
      let old_buffer = dst.buffer in
      resize_aux dst new_len; (* dst points to a new buffer now *)
      Bytes.blit old_buffer 0 dst.buffer 0 dstoff;
      Bytes.blit_string src srcoff dst.buffer dstoff len;
    end
    else Bytes.blit_string src srcoff dst.buffer dstoff len;
    if new_pos > dst.position then dst.position <- new_pos
;;

let blit_buffer src srcoff dst dstoff len =
  blit_substring_aux (Bytes.unsafe_to_string src.buffer) src.position srcoff dst dstoff len

let blit_substring src srcoff dst dstoff len =
  blit_substring_aux src (String.length src) srcoff dst dstoff len

let blit_subbytes src srcoff dst dstoff len =
  blit_substring_aux (Bytes.unsafe_to_string src) (Bytes.length src) srcoff dst dstoff len

let nth b ofs =
  if ofs < 0 || ofs >= b.position then
   invalid_arg "Buffer.nth"
  else Bytes.unsafe_get b.buffer ofs
;;

let length b = b.position

let clear b = b.position <- 0

let reset b =
  b.position <- 0; b.buffer <- b.initial_buffer;
  b.length <- Bytes.length b.buffer

let truncate b c =
  if c < 0 then invalid_arg "Buffer.truncate"
  else if b.position > c then b.position <- c

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b (pos + 1);
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset + len > String.length s
  then invalid_arg "Buffer.add_substring/add_subbytes";
  blit_substring_aux s (String.length s) offset b b.position len

let add_subbytes b s offset len =
  add_substring b (Bytes.unsafe_to_string s) offset len

let add_string b s =
  let len = String.length s in
  blit_substring_aux s len 0 b b.position len

let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

let add_buffer b bs =
  add_subbytes b bs.buffer 0 bs.position

(* read up to [len] bytes from [ic] into [b]. *)
let rec add_channel_rec b ic len =
  if len > 0 then (
    let n = input ic b.buffer b.position len in
    b.position <- b.position + n;
    if n = 0 then raise End_of_file
    else add_channel_rec b ic (len-n)   (* n <= len *)
  )

let add_channel b ic len =
  if len < 0 || len > Sys.max_string_length then   (* PR#5004 *)
    invalid_arg "Buffer.add_channel";
  if b.position + len > b.length then resize b (b.position + len);
  add_channel_rec b ic len

let output_buffer oc b =
  output oc b.buffer 0 b.position

let closing = function
  | '(' -> ')'
  | '{' -> '}'
  | _ -> assert false;;

(* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. *)
let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim then raise Not_found else
    if s.[i] = opening then advance (k + 1) (i + 1) lim else
    if s.[i] = closing then
      if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim in
  advance k start (String.length s);;

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim then lim else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> advance (i + 1) lim
    | _ -> i in
  advance start (String.length s);;

(* We are just at the beginning of an ident in s, starting at start. *)
let find_ident s start lim =
  if start >= lim then raise Not_found else
  match s.[start] with
  (* Parenthesized ident ? *)
  | '(' | '{' as c ->
     let new_start = start + 1 in
     let stop = advance_to_closing c (closing c) 0 s new_start in
     String.sub s new_start (stop - start - 1), stop + 1
  (* Regular ident *)
  | _ ->
     let stop = advance_to_non_alpha s (start + 1) in
     String.sub s start (stop - start), stop;;

(* Substitute $ident, $(ident), or ${ident} in s,
    according to the function mapping f. *)
let add_substitute b f s =
  let lim = String.length s in
  let rec subst previous i =
    if i < lim then begin
      match s.[i] with
      | '$' as current when previous = '\\' ->
         add_char b current;
         subst ' ' (i + 1)
      | '$' ->
         let j = i + 1 in
         let ident, next_i = find_ident s j lim in
         add_string b (f ident);
         subst ' ' next_i
      | current when previous == '\\' ->
         add_char b '\\';
         add_char b current;
         subst ' ' (i + 1)
      | '\\' as current ->
         subst current (i + 1)
      | current ->
         add_char b current;
         subst current (i + 1)
    end else
    if previous = '\\' then add_char b previous in
  subst ' ' 0;;
