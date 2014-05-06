(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

let nop _ = ()

external id: 'a -> 'a = "%identity"

let adapt_filename f =
  let change_suffix ext =
    try Filename.chop_extension f ^ ext
    with Invalid_argument _ -> f ^ ext
  in
  change_suffix
    (if Dynlink_common_interface.is_native then ".cmxs" else ".cmo")

(* [max_cpt t1 t2] returns the maximum of [t1] and [t2] wrt the total ordering
   induced by tags creation. This ordering is defined as follow:
   forall tags t1 t2,
   t1 <= t2 iff
   t1 is before t2 in the finite sequence
   [0; 1; ..; max_int; min_int; min_int-1; -1] *)
let max_cpt c1 c2 = max (c1 + min_int) (c2 + min_int) - min_int

let number_to_color n =
  let color = ref 0 in
  let number = ref n in
  for _i = 0 to 7 do
    color := (!color lsl 1) +
      (if !number land 1 <> 0 then 1 else 0) +
      (if !number land 2 <> 0 then 256 else 0) +
      (if !number land 4 <> 0 then 65536 else 0);
    number := !number lsr 3
  done;
  !color

(* ************************************************************************* *)
(** {2 Function builders} *)
(* ************************************************************************* *)

exception Unregistered_function of string 

let mk_labeled_fun s =
  raise 
    (Unregistered_function
       (Printf.sprintf "Function '%s' not registered yet" s))

let mk_fun s = ref (fun _ -> mk_labeled_fun s)

(* ************************************************************************* *)
(** {2 Function combinators} *)
(* ************************************************************************* *)

let ($) f g x = f (g x)

let swap f x y = f y x

let iter_uncurry2 iter f v =
  iter (fun a b -> f (a, b)) v

(* ************************************************************************* *)
(** {2 Lists} *)
(* ************************************************************************* *)

let as_singleton = function
  | [a] -> a
  | _ -> invalid_arg "Extlib.as_singleton"

let rec last = function
  | [] -> invalid_arg "Extlib.last"
  | [a] -> a
  | _ :: l -> last l

let filter_out f ls = List.filter (fun x -> not (f x)) ls

let replace cmp x l =
  let rec aux = function
    | [] -> [x]
    | y::l -> if cmp x y then x::l else y :: aux l
  in aux l

let filter_map filter f l =
  let rec aux = function
      [] -> []
    | x::tl -> if filter x then f x :: aux tl else aux tl
  in aux l
let filter_map' f filter l=
  let rec aux = function
    | [] -> []
    | x::tl -> let x' = f x in if filter x' then x' :: aux tl else aux tl
  in aux l

let product_fold f acc e1 e2 =
  List.fold_left
    (fun acc e1 -> List.fold_left (fun acc e2 -> f acc e1 e2) acc e2)
    acc e1

let product f e1 e2 = product_fold (fun acc e1 e2 -> f e1 e2 ::acc) [] e1 e2

let find_index f l =
  let rec aux i = function
      [] -> raise Not_found
    | x::l -> if f x then i else aux (i+1) l
  in aux 0 l

let rec list_compare cmp_elt l1 l2 =
  if l1 == l2 then 0
  else
    match l1, l2 with
      | [], [] -> assert false (* included in l1 == l2 above *)
      | [], _ :: _ -> 1
      | _ :: _, [] -> -1
      | v1::r1, v2::r2 ->
          let c = cmp_elt v1 v2 in
          if c = 0 then list_compare cmp_elt r1 r2 else c

let list_of_opt =
  function
    | None -> []
    | Some x -> [x]

let opt_of_list =
  function
    | [] -> None
    | [a] -> Some a
    | _ -> raise (Invalid_argument "Extlib.opt_of_list")

let rec find_opt f = function
  | [] -> raise Not_found
  | e :: q ->
      match f e with
        | None -> find_opt f q
        | Some v -> v

let iteri f l = let i = ref 0 in List.iter (fun x -> f !i x; incr i) l

let mapi f l =
  let res =
    snd (List.fold_left (fun (i,acc) x -> (i+1,f i x :: acc)) (0,[]) l)
  in List.rev res

(* ************************************************************************* *)
(** {2 Options} *)
(* ************************************************************************* *)

let has_some = function None -> false | Some _ -> true

let may f = function
  | None -> ()
  | Some x -> f x

(** [may_map f ?dft x] applies [f] to the value of [x] if exists. Otherwise
    returns the default value [dft].
    Assume that either [x] or [dft] is defined. *)
let may_map f ?dft x =
  match x, dft with
  | None, None -> assert false
  | None, Some dft -> dft
  | Some x, _ -> f x

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let opt_conv default = function
  | None -> default
  | Some x -> x

let opt_fold f o b =
  match o with
    | None -> b
    | Some a -> f a b

let merge_opt f k o1 o2 =
  match o1,o2 with
    | None, None -> None
    | Some x, None | None, Some x -> Some x
    | Some x1, Some x2 -> Some (f k x1 x2)

let opt_bind f = function
  | None -> None
  | Some x -> f x

let opt_filter f = function
  | None -> None
  | (Some x) as o -> if f x then o else None

let the = function None -> invalid_arg "Extlib.the" | Some x -> x

let find_or_none f v = try Some(f v) with Not_found -> None

let opt_equal f v1 v2 = match v1, v2 with
  | None, None -> true
  | Some _, None | None, Some _ -> false
  | Some v1, Some v2 -> f v1 v2

let opt_compare f v1 v2 = match v1, v2 with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some v1, Some v2 -> f v1 v2

(* ************************************************************************* *)
(** Booleans                                                                 *)
(* ************************************************************************* *)

let xor x y = if x then not y else y

(* ************************************************************************* *)
(** {2 Performance} *)
(* ************************************************************************* *)

external getperfcount: unit -> int = "getperfcount" "noalloc"
external getperfcount1024: unit -> int = "getperfcount1024" "noalloc"

let gentime counter ?msg f x =
  let c1 = counter () in
  let res = f x in
  let c2 = counter () in
  Format.printf "Time%s: %d@."
    (match msg with None -> "" | Some s -> " of " ^ s)
    (c2 - c1);
  res

let time ?msg f x = gentime getperfcount ?msg f x
let time1024 ?msg f x = gentime getperfcount1024 ?msg f x

(* The two functions below are not exported right now *)
let _time' name f =
  let cpt = ref 0 in
  fun x ->
    let b = getperfcount () in
    let res = f x in
    let e = getperfcount () in
    let diff = e - b in
    cpt := !cpt + diff;
    Format.eprintf "timing of %s: %d (%d)@." name !cpt diff;
    res

let _time2 name f =
  let cpt = ref 0 in
  fun x y ->
    let b = getperfcount () in
    let res = f x y in
    let e = getperfcount () in
    let diff = e - b in
    cpt := !cpt + diff;
    Format.eprintf "timing of %s: %d (%d)@." name !cpt diff;
    res

external address_of_value: 'a -> int = "address_of_value" "noalloc"

(* ************************************************************************* *)
(** {2 Exception catcher} *)
(* ************************************************************************* *)

let try_finally ~finally f x =
  try
    let r = f x in
    finally ();
    r
  with e ->
    finally ();
    raise e

(* ************************************************************************* *)
(** System commands *)
(* ************************************************************************* *)

let safe_remove f = try Unix.unlink f with Unix.Unix_error _ -> ()

let rec safe_remove_dir d =
  try
    Array.iter
      (fun a ->
         let f = Printf.sprintf "%s/%s" d a in
         if Sys.is_directory f then safe_remove_dir f else safe_remove f
      ) (Sys.readdir d) ;
    Unix.rmdir d
  with Unix.Unix_error _ | Sys_error _ -> ()

let cleanup_at_exit f = at_exit (fun () -> safe_remove f)

exception Temp_file_error of string

let temp_file_cleanup_at_exit ?(debug=false) s1 s2 =
  let file, out =
    try Filename.open_temp_file s1 s2
    with Sys_error s -> raise (Temp_file_error s)
  in
  (try close_out out with Unix.Unix_error _ -> ());
  at_exit 
    (fun () -> 
      if debug then begin
        (* If the caller decided to erase this file after all, don't
           print anything *)
        if Sys.file_exists file then
          Format.printf
            "@[[extlib] Debug flag was set: not removing file %s@]@." file;
      end else
        safe_remove file) ;
  file

let temp_dir_cleanup_at_exit ?(debug=false) base =
  let rec try_dir_cleanup_at_exit limit base =
    let file = Filename.temp_file base ".tmp" in
    let dir = Filename.chop_extension file ^ ".dir" in
    safe_remove file;
    try
      Unix.mkdir dir 0o700 ;
      at_exit
        (fun () ->
          if debug then begin
            if Sys.file_exists dir then
              Format.printf
                "@[[extlib] Debug flag was set: not removing dir %s@]@." dir;
          end else
            safe_remove_dir dir);
      dir
    with Unix.Unix_error _ ->
      if limit < 0 then
        let msg =
          Printf.sprintf "Impossible to create temporary directory ('%s')" dir
        in
        raise (Temp_file_error msg)
      else
        try_dir_cleanup_at_exit (pred limit) base
  in
  try_dir_cleanup_at_exit 10 base

external terminate_process: int -> unit = "terminate_process" "noalloc"
  (* In src/buckx/buckx_c.c *)

external usleep: int -> unit = "ml_usleep" "noalloc"
  (* In src/buckx/buckx_c.c ; man usleep for details. *)

(* ************************************************************************* *)
(** Strings *)
(* ************************************************************************* *)

let string_prefix ?(strict=false) prefix s =
  let add = if strict then 1 else 0 in
  String.length s >= String.length prefix + add
  && String.sub s 0 (String.length prefix) = prefix

let string_del_prefix ?(strict=false) prefix s =
  if string_prefix ~strict prefix s then
    Some 
      (String.sub s
         (String.length prefix) (String.length s - String.length prefix))
  else None

let string_split s i =
  let s1 = String.sub s 0 i in
  let s2 = String.sub s (i+1) (String.length s - i -1) in
  (s1,s2)

let make_unique_name mem ?(sep=" ") ?(start=2) from =
  let rec build base id =
    let fullname = base ^ sep ^ string_of_int id in
    if mem fullname then build base (succ id) else id,fullname
  in
  if mem from then build from start else (0,from)

(* ************************************************************************* *)
(** Comparison functions *)
(* ************************************************************************* *)

external compare_basic: 'a -> 'a -> int = "%compare"


let pretty_position fmt p =
  Format.fprintf fmt "<f:%s l:%d bol:%d c:%d>"
    p.Lexing.pos_fname p.Lexing.pos_lnum p.Lexing.pos_bol p.Lexing.pos_cnum

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
