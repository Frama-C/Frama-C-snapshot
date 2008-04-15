(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA for more details about the license.                       *)
(*                                                                        *)
(**************************************************************************)

(* $Id: lattice.ml,v 1.13 2008/04/01 09:25:22 uid568 Exp $ *)

module type S = sig
  include Abstract_interp.Lattice
  type annotation = string
  val possible_annotations : annotation list
  val annotations2state : t -> annotation list -> t

  val constant : t
  val variable : t 

  val use_ctrl_dependencies: bool
end

module Boolean(Str: sig val true_str: string val false_str: string end) = 
struct

  module B = struct
    type t = bool
    let to_string b = if b then Str.true_str else Str.false_str
    let pretty fmt b = Format.fprintf fmt "%s" (to_string b)
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    module Datatype = Datatype.Bool
  end

  include Abstract_interp.Make_Lattice_Base(B)

end

module Base = struct
 
  module S = struct let true_str = "public" let false_str = "private" end

  include Boolean(S)

  type annotation = string

  let possible_annotations = [ S.true_str; S.false_str ]

  let pub = inject true
  let priv = inject false

  let annotations2state dft = function
    | [] -> dft
    | [ s ] when s = S.true_str -> pub
    | [ s ] when s = S.false_str -> priv
    | _ -> assert false

  let constant = priv
  let variable =  priv

end

module Weak = struct
  include Base
  let use_ctrl_dependencies = false
end

module Strong = struct
  include Base
  let use_ctrl_dependencies = true
end

module Medium = struct

  module S = struct let true_str = "concrete" let false_str = "abstract" end
  module PP = Boolean(Base.S)
  module CA = Boolean(S)

  include 
  Abstract_interp.Make_Lattice_Product(PP)(CA)(struct let collapse = true end)

  type annotation = string

  let use_ctrl_dependencies = true

  let pub = PP.inject true
  let priv = PP.inject false
  let concr = CA.inject true
  let abstr = CA.inject false

  let possible_annotations = 
    S.true_str :: S.false_str :: Base.possible_annotations

  let annotations2state dft = 
    let rec aux = function
      | [] -> dft
      | s :: tl when s = Base.S.true_str -> inject pub (snd (aux tl))
      | s :: tl when s = Base.S.false_str -> inject priv (snd (aux tl))
      | s :: tl when s = S.true_str -> inject (fst (aux tl)) concr
      | s :: tl when s = S.false_str -> inject (fst (aux tl)) abstr
      | _ -> assert false
    in
    aux

  let constant = inject priv concr
  let variable = inject priv concr

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
