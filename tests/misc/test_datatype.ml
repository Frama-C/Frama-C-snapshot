module D = struct
  include Datatype.Int
  let name = "D"
  let structural_descr = Structural_descr.t_int
  let rehash x = x
  let copy = Datatype.undefined
end

module Test = Datatype.Make_with_collections(D)

module L = Datatype.List(D)

module M = Test.Map.Make(D)

module H = Test.Hashtbl.Make(D)

let main () =
  Format.printf "here@.";
  assert (L.copy [] == []);

  assert (Test.Set.equal (Test.Set.copy Test.Set.empty) Test.Set.empty);

  assert (M.equal (M.copy Test.Map.empty) Test.Map.empty);

  let h = Test.Hashtbl.create 3 in
  (* no equality in Hashtbls. *)
  Test.Hashtbl.iter (fun _ _ -> assert false) (H.copy h)

let () = Db.Main.extend main
