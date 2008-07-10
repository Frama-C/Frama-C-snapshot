(** Type variables: unification, generalization, ... *)

(** The type of type variables. *)
type t

(** Obtain a fresh type variable. *)
val fresh: string -> t
(** The argument is the name of the variable.
It can be anything, it shall only be used for pretty-printing purposes. *)

(** Find the value associated to a type variable. *)
val find: t -> t list -> 'a list -> 'a
(** Useful for type parameters.

[find v params param_values]: if [List.nth params i = v], return
[List.nth param_values i]. *)

(** Unique ID of a variable, different for each variable, even if it is
quantified. *)
val uid: t -> int

(** The name of a variable, which should only be used for
pretty-printing purposes. *)
val name: t -> string

(** The unique name of a variable, which is composed of its name and its UID. *)
val uname: t -> string
