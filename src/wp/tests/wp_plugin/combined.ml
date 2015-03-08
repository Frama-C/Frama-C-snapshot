let main () =
  Ast.compute ();
  let module OLS = Datatype.List(Datatype.String) in
  let module OKF = Datatype.Option(Kernel_function) in
  let module OP = Datatype.Option(Property) in
  Dynamic.get
    ~plugin:"Wp" "wp_compute"
    (Datatype.func3 OKF.ty OLS.ty OP.ty Datatype.unit)
    (Some
       (try Globals.Functions.find_by_name "job"
        with Not_found -> assert false))
    []
    None;
  let report =
    Dynamic.get
      ~plugin:"Report" "print" (Datatype.func Datatype.unit Datatype.unit)
  in
  report ();
  !Db.Value.compute ();
  report ()

let () = Db.Main.extend main
