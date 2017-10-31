(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* Configuration of the abstract domain. *)

type config = {
  cvalue : bool;
  equalities : bool;
  symbolic_locs : bool;
  bitwise : bool;
  gauges : bool;
  apron_oct : bool;
  apron_box : bool;
  polka_loose : bool;
  polka_strict : bool;
  polka_equalities : bool;
  inout: bool;
  signs: bool;
}

let configure () = {
  cvalue = Value_parameters.CvalueDomain.get ();
  equalities = Value_parameters.EqualityDomain.get ();
  symbolic_locs = Value_parameters.SymbolicLocsDomain.get ();
  bitwise = Value_parameters.BitwiseOffsmDomain.get ();
  gauges = Value_parameters.GaugesDomain.get ();
  apron_oct = Value_parameters.ApronOctagon.get ();
  apron_box = Value_parameters.ApronBox.get ();
  polka_loose = Value_parameters.PolkaLoose.get ();
  polka_strict = Value_parameters.PolkaStrict.get ();
  polka_equalities = Value_parameters.PolkaEqualities.get ();
  inout = Value_parameters.InoutDomain.get ();
  signs = Value_parameters.SignDomain.get ();
}

let default_config = configure ()

let legacy_config = {
  cvalue = true;
  equalities = false;
  symbolic_locs = false;
  bitwise = false;
  gauges = false;
  apron_oct = false;
  apron_box = false;
  polka_loose = false;
  polka_strict = false;
  polka_equalities = false;
  inout = false;
  signs = false;
}

module type Value = sig
  include Abstract_value.External
  val reduce : t -> t
end

module type S = sig
  module Val : Value
  module Loc : Abstract_location.External with type value = Val.t
                                           and type location = Precise_locs.precise_location
  module Dom : Abstract_domain.External with type value = Val.t
                                         and type location = Loc.location
end


(* -------------------------------------------------------------------------- *)
(*                           Value Abstraction                                *)
(* -------------------------------------------------------------------------- *)

module type V = sig
  include Abstract_value.External
  val structure : t Abstract_value.structure
end

module CVal = struct
  include Main_values.CVal
  include Structure.Open (Structure.Key_Value) (Main_values.CVal)
end

let has_apron config =
  config.apron_oct || config.apron_box || config.polka_equalities
  || config.polka_loose || config.polka_strict

(* The apron domains relies on a specific interval abstraction to communicate
   with other domains. This function adds the intervals to the current [value]
   abstraction. These intervals carry the same information as the cvalue
   abstractions (if they are enabled). Do not display the intervals in the GUI
   in this case. *)
let add_apron_value config value =
  let module Left = ((val value: Abstract_value.Internal)) in
  let module V = struct
    include Value_product.Make (Left) (Main_values.Interval)
    let pretty_typ =
      if config.cvalue
      then fun fmt typ (left, _right) -> Left.pretty_typ fmt typ left
      else pretty_typ
  end in
  (module V: Abstract_value.Internal)

let open_value_abstraction value =
  let module Value = (val value : Abstract_value.Internal) in
  (module struct
    include Value
    include Structure.Open (Structure.Key_Value) (Value)
  end : V)

let build_value config =
  let value =
    if config.bitwise
    then (module Offsm_value.CvalueOffsm : Abstract_value.Internal)
    else (module Main_values.CVal : Abstract_value.Internal)
  in
  let value =
    if config.signs
    then
      let module V = Value_product.Make ((val value)) (Sign_value) in
      (module V: Abstract_value.Internal)
    else value
  in
  let value =
    if has_apron config
    then add_apron_value config value
    else value
  in
  open_value_abstraction value

(* Builds a module conversion from a generic external value to a key. *)
module Convert
    (Value : Abstract_value.External)
    (K : sig type v val key : v Abstract_value.key end)
= struct
  type extended_value = Value.t
  type extended_location = Main_locations.PLoc.location

  let extend_val =
    let set = Value.set K.key in
    fun v -> set v Value.top

  let restrict_val = match Value.get K.key with
    | None -> assert false
    | Some get -> get

  let restrict_loc = fun x -> x
  let extend_loc = fun x -> x
end


(* -------------------------------------------------------------------------- *)
(*                              Cvalue Domain                                 *)
(* -------------------------------------------------------------------------- *)

(* Abstractions needed for the analysis: value, location and domain. *)
module type Abstract = sig
  module Val : V
  module Loc : Abstract_location.Internal with type value = Val.t
                                           and type location = Precise_locs.precise_location
  module Dom : Abstract_domain.Internal with type value = Val.t
                                         and type location = Loc.location
end

let default_root_abstraction config =
  if config.cvalue
  then
    (module struct
      module Val = CVal
      module Loc = Main_locations.PLoc
      module Dom = Cvalue_domain.State
    end : Abstract)
  else
    (module struct
      module Val = CVal
      module Loc = Main_locations.PLoc
      module Dom = Unit_domain.Make (Val) (Loc)
    end : Abstract)

let build_root_abstraction config value =
  let module Val = (val value : V) in
  let module K = struct
    type v = Cvalue.V.t
    let key = Main_values.cvalue_key
  end in
  let module Conv = Convert (Val) (K) in
  if config.cvalue
  then
    (module struct
      module Val = Val
      module Loc = Location_lift.Make (Main_locations.PLoc) (Conv)
      module Dom = Domain_lift.Make (Cvalue_domain.State) (Conv)
    end : Abstract)
  else
    (module struct
      module Val = Val
      module Loc = Location_lift.Make (Main_locations.PLoc) (Conv)
      module Dom = Unit_domain.Make (Val) (Loc)
    end : Abstract)


(* -------------------------------------------------------------------------- *)
(*                              Apron Domains                                 *)
(* -------------------------------------------------------------------------- *)

let add_apron_domain abstract apron =
  let module Abstract = (val abstract: Abstract) in
  let module K = struct
    type v = Main_values.Interval.t
    let key = Main_values.interval_key
  end in
  let module Conv = Convert (Abstract.Val) (K) in
  let module Apron = Domain_lift.Make ((val apron : Apron_domain.S)) (Conv) in
  (module struct
    module Val = Abstract.Val
    module Loc = Abstract.Loc
    module Dom = Domain_product.Make (Abstract.Val) (Abstract.Dom) (Apron)
  end : Abstract)

let dkey_experimental = Value_parameters.register_category "experimental-ok"

let add_apron_domain abstractions apron =
  if not (Value_parameters.is_debug_key_enabled dkey_experimental) then
    Value_parameters.warning  "The Apron domains binding is experimental.";
  if Apron_domain.ok
  then add_apron_domain abstractions apron
  else
    Value_parameters.abort
      "Apron domain requested but apron binding not available: analysis aborted."


(* -------------------------------------------------------------------------- *)
(*                            Equality Domain                                 *)
(* -------------------------------------------------------------------------- *)

module CvalueEquality = Equality_domain.Make (CVal)

let add_generic_equalities (module Abstract : Abstract) =
  let module EqDom = Equality_domain.Make (Abstract.Val) in
  let module Dom = Domain_product.Make (Abstract.Val) (Abstract.Dom) (EqDom) in
  (module struct
    module Val = Abstract.Val
    module Loc = Abstract.Loc
    module Dom = Dom
  end : Abstract)

let add_equalities (type v) (module Abstract : Abstract with type Val.t = v) =
  match Abstract.Val.structure with
  | Structure.Key_Value.Leaf key ->
    begin
      match Structure.Key_Value.eq_type key Main_values.cvalue_key with
      | None -> add_generic_equalities (module Abstract)
      | Some Structure.Eq ->
        let module Dom =
          Domain_product.Make (Abstract.Val) (Abstract.Dom) (CvalueEquality)
        in
        (module struct
          module Val = Abstract.Val
          module Loc = Abstract.Loc
          module Dom = Dom
        end : Abstract)
    end
  | _ -> add_generic_equalities (module Abstract)


(* -------------------------------------------------------------------------- *)
(*                            Offsetmap Domain                                *)
(* -------------------------------------------------------------------------- *)

let add_offsm abstract =
  let module Abstract = (val abstract : Abstract) in
  let module K = struct
    type v = Offsm_value.offsm_or_top
    let key = Offsm_value.offsm_key
  end in
  let module Conv = Convert (Abstract.Val) (K) in
  let module Offsm = Domain_lift.Make (Offsm_domain.D) (Conv) in
  let module Dom = Domain_product.Make (Abstract.Val) (Abstract.Dom) (Offsm) in
  (module struct
    module Val = Abstract.Val
    module Loc = Abstract.Loc
    module Dom = Dom
  end : Abstract)

(* -------------------------------------------------------------------------- *)
(*                   Domains on standard locations and values                 *)
(* -------------------------------------------------------------------------- *)

module type Standard_abstraction = Abstract_domain.Internal
  with type value = Cvalue.V.t
   and type location = Precise_locs.precise_location

let add_standard_domain d abstract =
  let module Abstract = (val abstract : Abstract) in
  let module K = struct
    type v = Cvalue.V.t
    let key = Main_values.cvalue_key
  end in
  let module Conv = Convert (Abstract.Val) (K) in
  let module D = (val d: Standard_abstraction) in
  let module LD = Domain_lift.Make (D) (Conv) in
  let module Dom = Domain_product.Make (Abstract.Val)(Abstract.Dom)(LD) in
  (module struct
    module Val = Abstract.Val
    module Loc = Abstract.Loc
    module Dom = Dom
  end : Abstract)

(* List of abstractions registered by other plugins *)
let dynamic_abstractions = ref []

let add_dynamic_abstractions abstract =
  List.fold_left
    (fun d abstract -> add_standard_domain abstract d)
    abstract !dynamic_abstractions

let register_dynamic_abstraction d =
  dynamic_abstractions := d :: !dynamic_abstractions

(* --------------------------------------------------------------------------*)
(*                            Symbolic locations                             *)
(* --------------------------------------------------------------------------*)

let add_symbolic_locs =
  add_standard_domain (module Symbolic_locs.D)

(* -------------------------------------------------------------------------- *)
(*                            Gauges                                          *)
(* -------------------------------------------------------------------------- *)

let add_gauges =
  add_standard_domain (module Gauges_domain.D)

(* -------------------------------------------------------------------------- *)
(*                            Inout                                           *)
(* -------------------------------------------------------------------------- *)

let add_inout =
  add_standard_domain (module Inout_domain.D)

(* -------------------------------------------------------------------------- *)
(*                            Sign Domain                                     *)
(* -------------------------------------------------------------------------- *)

let add_signs abstract =
  let module Abstract = (val abstract : Abstract) in
  let module K = struct
    type v = Sign_value.t
    let key = Sign_value.sign_key
  end in
  let module Conv = Convert (Abstract.Val) (K) in
  let module Sign = Domain_lift.Make (Sign_domain) (Conv) in
  let module Dom = Domain_product.Make (Abstract.Val) (Abstract.Dom) (Sign) in
  (module struct
    module Val = Abstract.Val
    module Loc = Abstract.Loc
    module Dom = Dom
  end : Abstract)


(* -------------------------------------------------------------------------- *)
(*                            Build Abstractions                              *)
(* -------------------------------------------------------------------------- *)

let build_abstractions config =
  let value = build_value config in
  let module V = (val value : V) in
  let abstractions =
    match V.structure with
    | Structure.Key_Value.Leaf key
      when Structure.Key_Value.equal key Main_values.cvalue_key ->
      default_root_abstraction config
    | _ -> build_root_abstraction config value
  in
  let abstractions =
    if config.apron_oct
    then add_apron_domain abstractions (module Apron_domain.Octagon)
    else abstractions
  in
  let abstractions =
    if config.apron_box
    then add_apron_domain abstractions (module Apron_domain.Box)
    else abstractions
  in
  let abstractions =
    if config.polka_loose
    then add_apron_domain abstractions (module Apron_domain.Polka_Loose)
    else abstractions
  in
  let abstractions =
    if config.polka_strict
    then add_apron_domain abstractions (module Apron_domain.Polka_Strict)
    else abstractions
  in
  let abstractions =
    if config.polka_equalities
    then add_apron_domain abstractions (module Apron_domain.Polka_Equalities)
    else abstractions
  in
  let module A = (val abstractions : Abstract) in
  let abstractions =
    if config.equalities
    then add_equalities (module A)
    else abstractions
  in
  let abstractions =
    if config.symbolic_locs
    then add_symbolic_locs abstractions
    else abstractions
  in
  let abstractions =
    if config.bitwise
    then add_offsm abstractions
    else abstractions
  in
  let abstractions =
    if config.gauges
    then add_gauges abstractions
    else abstractions
  in
  let abstractions =
    if config.inout
    then add_inout abstractions
    else abstractions
  in
  let abstractions =
    if config.signs
    then add_signs abstractions
    else abstractions
  in
  let abstractions = add_dynamic_abstractions abstractions in
  abstractions


(* Add the reduce function to the value module. *)
module Reduce (Value : Abstract_value.External) = struct

  include Value

  (* When the value abstraction contains both a cvalue and an interval
     component (coming currently from an Apron domain), reduce them from each
     other. If the Cvalue is not a scalar do nothing, because we do not
     currently use Apron for pointer offsets. *)
  let reduce =
    match Value.get Main_values.interval_key, Value.get Main_values.cvalue_key with
    | Some get_interval, Some get_cvalue ->
      begin
        let set_cvalue = Value.set Main_values.cvalue_key in
        let set_interval = Value.set Main_values.interval_key in
        fun t ->
          match get_interval t with
          | None -> begin
              let cvalue = get_cvalue t in
              try
                let ival = Cvalue.V.project_ival cvalue in
                set_interval (Some ival) t
              with Cvalue.V.Not_based_on_null -> t
            end
          | Some ival ->
            let cvalue = get_cvalue t in
            try
              let ival' = Cvalue.V.project_ival cvalue in
              (match ival' with
               | Ival.Float _ -> raise Cvalue.V.Not_based_on_null
               | _ -> ());
              let reduced_ival = Ival.narrow ival ival' in
              let cvalue = Cvalue.V.inject_ival reduced_ival in
              set_interval (Some reduced_ival) (set_cvalue cvalue t)
            with Cvalue.V.Not_based_on_null -> t
      end
    | _, _ -> fun x -> x

end

let open_abstractions abstraction =
  let module Abstract = (val abstraction : Abstract) in
  let module Val = Reduce (Abstract.Val) in
  let module Loc = struct
    include Abstract.Loc
    include Structure.Open
        (Structure.Key_Location)
        (struct include Abstract.Loc type t = location end)
  end in
  let module Domain = struct
    include Abstract.Dom
    include Structure.Open (Structure.Key_Domain) (Abstract.Dom)
  end in
  (module struct
    module Val = Val
    module Loc = Loc
    module Dom = Domain
  end : S)


let make config =
  let abstractions = build_abstractions config in
  open_abstractions abstractions


(* -------------------------------------------------------------------------- *)
(*                       Default and Legacy Abstractions                      *)
(* -------------------------------------------------------------------------- *)


module Legacy = struct

  module Val = struct
    include Main_values.CVal
    include Structure.Open (Structure.Key_Value) (Main_values.CVal)
    let reduce t = t
  end

  module Loc = struct
    include Main_locations.PLoc
    include Structure.Open
        (Structure.Key_Location)
        (struct include Main_locations.PLoc type t = location end)
  end

  module Dom = struct
    include Cvalue_domain.State
    include Structure.Open (Structure.Key_Domain) (Cvalue_domain.State)
  end

end

module Default = (val make default_config)



(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
