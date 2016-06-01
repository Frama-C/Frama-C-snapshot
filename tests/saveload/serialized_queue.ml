module Q =
  State_builder.Queue
    (Datatype.Int)
    (struct let name = "Queue.Q" let dependencies = [] end)

let pretty () =
  Kernel.feedback "Content of queue:";
  Q.iter (fun x -> Kernel.feedback "%d" x)

let run () =
  Q.add 1; Q.add 2;
  pretty();
  let prj = Project.create "new project" in
  let queue_ops = State.private_ops Q.self in
  let obj = queue_ops.State.serialize (Project.current()) in
  let () = queue_ops.State.unserialize prj obj in
  Project.on prj pretty ();
  Project.on prj Q.add 3;
  Project.on prj pretty ()

let () = run ()
  
