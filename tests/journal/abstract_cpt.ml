let mk () = ref 0
let incr c = incr c; !c

include Datatype.Make(struct
  (* order of lines below does matter *)
  include Datatype.Serializable_undefined
  include Datatype.Ref(Datatype.Int)
  let varname _ = "cpt"
  let name = "Abstract_cpt.t"
end)

let mk =
  Dynamic.register
    ~journalize:true
    ~plugin:"Abstract_cpt"
    "mk"
    (Datatype.func Datatype.unit ty) mk

let incr =
  Dynamic.register
    ~journalize:true
    ~plugin:"Abstract_cpt"
    "incr"
    (Datatype.func ty Datatype.int)
    incr

let pretty =
  Dynamic.register
    ~journalize:true
    ~plugin:"Abstract_cpt"
    "pretty"
    (Datatype.func ty Datatype.unit)
    (fun n -> Format.printf "%d@." !n)
