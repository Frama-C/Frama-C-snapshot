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

type tt = {
  over_inputs: Locations.Zone.t;
  over_inputs_if_termination: Locations.Zone.t;
  under_outputs_if_termination: Locations.Zone.t;
  over_outputs: Locations.Zone.t;
  over_outputs_if_termination: Locations.Zone.t;
}

let pretty_operational_inputs_aux fmt x =
  Format.fprintf fmt "@[<v 2>Operational inputs:@ @[<hov>%a@]@]@ "
    Locations.Zone.pretty (x.over_inputs);
  Format.fprintf fmt "@[<v 2>Operational inputs on termination:@ @[<hov>%a@]@]@ "
    Locations.Zone.pretty (x.over_inputs_if_termination);
  Format.fprintf fmt "@[<v 2>Sure outputs:@ @[<hov>%a@]@]@ "
    Locations.Zone.pretty (x.under_outputs_if_termination);
;;

let pretty_outputs_aux fmt x =
  Format.fprintf fmt "@[<v 2>Over outputs:@ @[<hov>%a@]@]@ "
    Locations.Zone.pretty (x.over_outputs);
  Format.fprintf fmt "@[<v 2>Over outputs on termination:@ @[<hov>%a@]@]@ "
    Locations.Zone.pretty (x.over_outputs_if_termination);
;;

let wrap_vbox f fmt x =
  Format.fprintf fmt "@[<v>";
  f fmt x;
  Format.fprintf fmt "@]"

let pretty_operational_inputs = wrap_vbox pretty_operational_inputs_aux
let pretty_outputs = wrap_vbox pretty_outputs_aux

open Locations

include Datatype.Make
(struct
  include Datatype.Serializable_undefined
  type t = tt
    let pretty fmt x =
      Format.fprintf fmt "@[<v>";
      pretty_operational_inputs_aux fmt x;
      pretty_outputs_aux fmt x;
      Format.fprintf fmt "@]"

    let structural_descr =
      let z = Locations.Zone.packed_descr in
      Structural_descr.t_record [| z; z; z; z; z |]
    let reprs =
      List.map
        (fun z ->
          { over_inputs_if_termination = z;
            under_outputs_if_termination = z;
            over_inputs = z;
            over_outputs = z;
            over_outputs_if_termination = z;
          }) Locations.Zone.reprs
    let name = "Full.tt"
    let hash
        { over_inputs_if_termination = a;
          under_outputs_if_termination = b;
          over_inputs = c;
          over_outputs = d;
          over_outputs_if_termination = e;
        } =
      Zone.hash a + 17 * Zone.hash b + 587 * Zone.hash c + 1077 * Zone.hash d + 13119 * Zone.hash e
    let equal
        { over_inputs_if_termination = a;
          under_outputs_if_termination = b;
          over_inputs = c;
          over_outputs = d;
          over_outputs_if_termination = e;
        }
        { over_inputs_if_termination = a';
          under_outputs_if_termination = b';
          over_inputs = c';
          over_outputs = d';
          over_outputs_if_termination = e';
        } =
      Zone.equal a a' && Zone.equal b b' && Zone.equal c c' && Zone.equal d d' && Zone.equal e e'
    let mem_project = Datatype.never_any_project
 end)

let map f v = {
  over_inputs_if_termination = f v.over_inputs_if_termination;
  under_outputs_if_termination = f v.under_outputs_if_termination;
  over_inputs = f v.over_inputs;
  over_outputs = f v.over_outputs;
  over_outputs_if_termination = f v.over_outputs_if_termination;
}

let bottom = {
  over_inputs = Zone.bottom;
  over_inputs_if_termination = Zone.bottom;
  under_outputs_if_termination = Zone.top;
  over_outputs = Zone.bottom;
  over_outputs_if_termination = Zone.bottom;
}

let join c1 c2 = {
  over_inputs = Zone.join c1.over_inputs c2.over_inputs;
  over_inputs_if_termination =
    Zone.join c1.over_inputs_if_termination c2.over_inputs_if_termination;
  over_outputs = Zone.join c1.over_outputs c2.over_outputs;
  over_outputs_if_termination =
    Zone.join c1.over_outputs_if_termination c2.over_outputs_if_termination;
  under_outputs_if_termination =
    Zone.meet c1.under_outputs_if_termination c2.under_outputs_if_termination;
}


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
