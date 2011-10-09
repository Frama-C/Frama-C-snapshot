(** Logic Path & Regions *)

open LogicTau
open LogicLang

val basename : tau -> string
val fresh : pool -> tau -> var
val e_shared : pool -> tau -> term -> (term -> term) -> term
val p_shared : pool -> tau -> term -> (term -> pred) -> pred
