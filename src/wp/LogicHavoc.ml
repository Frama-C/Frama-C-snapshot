(* -------------------------------------------------------------------------- *)
(* --- Havoc                                                              --- *)
(* -------------------------------------------------------------------------- *)

open LogicId
open LogicTau
open LogicLang

type region =
  | Full
  | Empty
  | Field of field_region list (* SORTED & MERGED *)
  | Index of tau list * index_region list (* SAME SIGNATURE *)

and field_region = field * region
and index_region = ( term list -> pred ) * region (* FULL or FIELD region *)

(* -------------------------------------------------------------------------- *)
(* --- Merge Operations                                                   --- *)
(* -------------------------------------------------------------------------- *)

let rec merge_signature ts1 ts2 =
  match ts1,ts2 with
    | [],ts | ts,[] -> ts
    | t1::ts1 , t2::ts2 ->
	if compare_tau t1 t2 <> 0 then 
	  failwith "merge incompatible index during havoc" ;
	t1 :: merge_signature ts1 ts2

let rec merge r1 r2 =
  match r1 , r2 with
    | Full , _ | _ , Full -> Full
    | Empty , r | r , Empty -> r
    | Field _ , Index _ | Index _ , Field _ -> 
	failwith "merge field and index during havoc"
    | Field fs1 , Field fs2 -> Field (merge_fields fs1 fs2)
    | Index (ts1,ks1) , Index (ts2,ks2) ->
	let ts = merge_signature ts1 ts2 in
	Index( ts , ks1 @ ks2 ) (* Extension is natural *)
  
and merge_fields fs1 fs2 =
  match fs1 , fs2 with
    | [] , fs | fs , [] -> fs
    | ((f1,r1) as h1)::ftail1 , ((f2,r2) as h2)::ftail2 ->
	let cmp = compare_field f1 f2 in
	if cmp < 0 then h1 :: merge_fields ftail1 fs2 else
	  if cmp > 0 then h2 :: merge_fields fs1 ftail2 else
	    (f1 , merge r1 r2) :: merge_fields ftail1 ftail2

(* -------------------------------------------------------------------------- *)
(* --- Constructors                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rec shift d = function
  | _::xs when d>0 -> shift (pred d) xs
  | xs -> xs

let fsort (f1,_) (f2,_) = compare_field f1 f2

let empty = Empty
let full = Full

let field fs f r =
  if r = Empty then Empty else
    Field (List.map 
	     (fun g -> g , if compare_field f g = 0 then r else Empty)
	     (List.sort compare_field fs))
      
let fields frs = 
  if List.for_all (fun (_,r) -> r = Empty) frs then Empty else
    if List.for_all (fun (_,r) -> r = Full) frs then Full else
      Field (List.sort fsort frs)

let matrix ts cond = function
  | Empty -> Empty
  | (Field _ | Full) as r -> Index(ts,[cond,r])
  | Index(ts0,kregions) ->
      let d = List.length ts in
      let gregions = List.map
	(fun (cond0,r0) ->
	   let gcond = fun xs -> p_and (cond xs) (cond0 (shift d xs)) in
	   gcond , r0)
	kregions
      in
      Index( ts @ ts0 , gregions )

let array t cond = function
  | Empty -> Empty
  | region -> matrix [t] (fun ts -> cond (List.hd ts)) region

let in_range a b k = p_and (p_icmp Cleq a k) (p_icmp Cleq k b)
let index term r = array Integer (p_equal term) r
let range a b r = array Integer (in_range a b) r

(* -------------------------------------------------------------------------- *)
(* --- Havoc Relation                                                     --- *)
(* -------------------------------------------------------------------------- *)

let forall xs p = List.fold_right p_forall xs p
let access a ks = List.fold_left e_access a ks

let only_one_region vs kregions : ( pred * region ) list =
  let kregions = Array.of_list kregions in
  Array.to_list
    (Array.mapi
       (fun i (_,region) ->
	  let conds_i =
	    Array.mapi
	      (fun j (cond,_) -> 
		 let p = cond vs in
		 if i=j then p else p_not p)
	      kregions
	  in
	  p_conj (Array.to_list conds_i) , region
       ) kregions)

let rec is_havoc pool x1 x2 = function
  | Empty -> p_equal x1 x2
  | Full -> p_true
  | Field fs ->
      List.fold_left 
	(fun w (f,r) -> 
	   p_and w (is_havoc pool (e_getfield x1 f) (e_getfield x2 f) r))
	p_true fs
  | Index(ts,ks) ->
      let xs = List.map (LogicLib.fresh pool) ts in
      let vs = List.map e_var xs in
      let a1_xs = access x1 vs in
      let a2_xs = access x2 vs in
      let all_diff_then_equal =
	forall xs 
	  (p_implies
	     (p_conj (List.map (fun (cond,_) -> p_not (cond vs)) ks))
	     (p_equal a1_xs a2_xs)) in
      let only_once_then_region =
	List.map
	  (fun (condition,region) ->
	     forall xs (p_implies condition (is_havoc pool a1_xs a2_xs region))
	  ) (only_one_region vs ks) in
      p_conj (all_diff_then_equal :: only_once_then_region)
