(* $Id: extlib.ml,v 1.5 2008/02/18 13:54:38 uid570 Exp $ *)

exception NotYetImplemented of string
let not_yet_implemented s = raise (NotYetImplemented s)
let mk_fun s = ref (fun _ -> not_yet_implemented s)

let nop _ = ()

let ($) f g x = f (g x)

let swap f x y = f y x

let find_or_none f v = try Some(f v) with Not_found -> None

(* ************************************************************************** *)
(** {2 Lists} *)
(* ************************************************************************** *)

let rec last = function
  | [] -> invalid_arg "Extlib.last"
  | [ a ] -> a
  | _ :: tl -> last tl

let as_singleton = function
  | [a] -> a
  | _ -> invalid_arg "Extlib.as_singleton"

let filter_out f ls = List.filter (fun x -> not (f x)) ls

(* ************************************************************************** *)
(** {2 Options} *)
(* ************************************************************************** *)

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

let the = function None -> invalid_arg "Extlib.the" | Some x -> x

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
