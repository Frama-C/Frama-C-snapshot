(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

type offsm_or_top = O of Cvalue.V_Offsetmap.t | Top

val offsm_key : offsm_or_top Structure.Key_Value.k

val cast :
  old_size: Integer.t -> new_size: Integer.t -> signed: bool ->
  Cvalue.V_Offsetmap.t -> Cvalue.V_Offsetmap.t


module Offsm : Abstract_value.Internal with type t = offsm_or_top

module CvalueOffsm : Abstract_value.Internal with type t = Cvalue.V.t * offsm_or_top
