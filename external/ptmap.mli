(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Menhir                               *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in the file licences/Q_MODIFIED_LICENSE.             *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'Énergie Atomique).             *)
(**************************************************************************)

(* $Id: ptmap.mli,v 1.4 2008/10/10 13:27:07 uid527 Exp $ *)

module type Tagged_type =
sig
  type t
  val tag : t -> int
  val equal : t -> t -> bool
  val pretty : Format.formatter -> t -> unit
  module Datatype : Project.Datatype.S with type t = t
end

module Generic 
  (X:sig 
    type t
    val name : string
    val id: t -> int
    val pretty: Format.formatter -> t -> unit
  end) 
  (V : Tagged_type) 
  (Initial_Values : sig val v : (X.t*V.t) list list end) :
sig 
  
  type t

  type key = X.t
      
  val empty : t

  val tag : t -> int
  val hash_debug : t -> int

(*  val compare : t -> t -> int *)
  val equal : t -> t -> bool
  val is_empty : t -> bool
    
  val add : key -> V.t -> t -> t
    
  val find : key -> t -> V.t
    
  val remove : key -> t -> t
    
  (*val mem :  int -> t -> bool*)

  val iter : (X.t -> V.t -> unit) -> t -> unit
    
  val map : (V.t -> V.t) -> t -> t
    
  (*val mapi : (int -> 'a -> 'b) -> t -> 'b t*)
    
  val fold : (X.t -> V.t -> 'b -> 'b) -> t -> 'b -> 'b
    
  (*val compare : ('a -> 'a -> int) -> t -> t -> int*)
    
  (*val equal : ('a -> 'a -> bool) -> t -> t -> bool*)
    
  val generic_merge : cache:(string * int) -> 
    decide:(X.t -> V.t option -> V.t option -> V.t) -> t -> t -> t

  val symetric_merge : cache:(string * int) -> 
    decide_none:(X.t -> V.t -> V.t) ->
      decide_some:(V.t -> V.t -> V.t) -> t -> t -> t

  val generic_is_included : exn -> cache:(string * int) -> 
    decide_fst:(X.t -> V.t  -> unit) -> 
    decide_snd:(X.t -> V.t  -> unit) -> 
    decide_both:(V.t -> V.t -> unit) -> t -> t -> unit

  val cached_fold :    
    cache:string * int ->
    f:(key -> V.t -> 'b) ->
    joiner:('b -> 'b -> 'b) -> empty:'b -> t -> 'b

  val cached_map :
    cache:string * int ->
    f:(key -> V.t -> V.t) -> t -> t

  module Datatype : Project.Datatype.S with type t = t

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C .. -j"
End:
*)
