open Bigarray
open Unix
let compare_files f f' = 
  let fd = Unix.openfile f [Unix.O_RDONLY] 0o000 in
  let fd' = Unix.openfile f' [Unix.O_RDONLY] 0o000 in
  let size_byte = (Unix.fstat fd).st_size in
  let size_byte' = (Unix.fstat fd').st_size in
  if size_byte' <> size_byte then false
  else
    (try 
       let initial_padding = size_byte mod 8 in
       for i = 1 to initial_padding do
         let s = "_" in 
         let s' = "_" in
         assert (Unix.read fd s 0 1=1); 
         assert (Unix.read fd' s' 0 1=1);
         if s <> s' then raise Not_found
       done;
       let size_bigarray = size_byte / 8 in 
       let mapped = Array1.map_file fd int64 c_layout false size_bigarray in
       let mapped' = Array1.map_file fd' int64 c_layout false size_bigarray in
       mapped = mapped'
     with Not_found -> false)

let () = Format.printf "GOT:%b@." (compare_files "/tmp/big.mmap" "/tmp/big.mmap")
