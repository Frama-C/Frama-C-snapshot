include
  Plugin.Register
    (struct
      let name = "a"
      let shortname = "a"
      let help = ""
    end)

module M =
  String_multiple_map
    (struct
      include Datatype.Integer
      type key = string
      let of_string ~key:_ ~prev:_ arg =
        try
          Extlib.opt_map Integer.of_string arg
        with Failure _ ->
          raise (Cannot_build "expecting an integer")
      let to_string ~key:_ = Extlib.opt_map Integer.to_string
    end)
    (struct
      let option_name = "-multiple-map"
      let help = ""
      let default = Datatype.String.Map.empty
      let arg_name = "s:i"
    end)


let main () =
  let print k v =
    feedback "%s => %a" k (Pretty_utils.pp_list ~sep:";@," Integer.pretty) v
  in
  Datatype.String.Map.iter print (M.get ())

let () = Db.Main.extend main

