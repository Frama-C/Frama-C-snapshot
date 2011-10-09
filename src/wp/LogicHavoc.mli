
open LogicId
open LogicTau
open LogicLang

type region

val empty : region
val full : region

val fields : ( field * region ) list -> region
val field : field list -> field -> region -> region
  (** [field fs f r] is [empty] for all [fs] except [f] for which it is [r] *)

val index : term -> region -> region
val range : term -> term -> region -> region
val array : tau -> (term -> pred) -> region -> region
val matrix : tau list -> (term list -> pred) -> region -> region

val is_havoc : pool -> term -> term -> region -> pred

