open Jc_pervasives
open Jc_env
open Jc_ast
open Format
open Pp

let string_of_invariant_policy p =
  match p with
    | InvNone -> "None"
    | InvArguments -> "Arguments"
    | InvOwnership -> "Ownership"

let string_of_separation_policy p =
  match p with
    | SepNone -> "None"
    | SepRegions -> "Regions"

let string_of_annotation_policy p =
  match p with
    | AnnotNone -> "None"
    | AnnotInvariants -> "Invariants"
    | AnnotWeakPre -> "WeakPre"
    | AnnotStrongPre -> "StrongPre"

let string_of_abstract_domain p =
  match p with
    | AbsNone -> "None"
    | AbsBox -> "Box"
    | AbsOct -> "Oct"
    | AbsPol -> "Pol"

let string_of_int_model p =
  match p with
    | IMbounded -> "bounded"
    | IMmodulo -> "modulo"

let const fmt c =
  match c with
    | JCCinteger s -> fprintf fmt "%s" s
    | JCCreal s -> fprintf fmt "%s" s
    | JCCboolean b -> fprintf fmt "%B" b
    | JCCnull -> fprintf fmt "null"
    | JCCvoid -> fprintf fmt "()"
    | JCCstring s -> fprintf fmt "\"%s\"" s

let label fmt l =
  match l with
    | LabelName s -> fprintf fmt "%s" s.label_info_name
    | LabelHere -> fprintf fmt "Here" 
    | LabelPre -> fprintf fmt "Pre" 
    | LabelOld -> fprintf fmt "Old" 
    | LabelPost -> fprintf fmt "Post" 

let ptype fmt t =
  match t#node with
    | JCPTnative n -> fprintf fmt "%s" (string_of_native n)
    | JCPTidentifier s -> string fmt s
    | JCPTpointer (name,ao, bo) ->
	begin match ao, bo with
	  | None, None ->
	      fprintf fmt "%s[..]" name
	  | Some a, None ->
	      fprintf fmt "%s[%s..]" name (Num.string_of_num a)
	  | None, Some b ->
	      fprintf fmt "%s[..%s]" name (Num.string_of_num b)
	  | Some a, Some b ->
	      if Num.eq_num a b then
		fprintf fmt "%s[%s]" name (Num.string_of_num a)
	      else
		fprintf fmt "%s[%s..%s]" name
		  (Num.string_of_num a) (Num.string_of_num b)
	end

let offset_kind fmt k =
  match k with
    | Offset_max -> fprintf fmt "ax"
    | Offset_min -> fprintf fmt "in"

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
