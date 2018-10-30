(* Performs syntactic checks related to parseability of Frama-C libc headers *)

let run_once = ref false

let header_re = Str.regexp ".*\\.h$"

let is_header f = Str.string_match header_re f 0

(* Files which are *not* supposed to be parsed *)
let blacklist () =
  let libc = (Filename.concat (Sys.getenv "FRAMAC_SHARE") "libc") in
  List.map (fun f ->
      Datatype.Filepath.of_string (Filename.concat libc f))
    ["tgmath.h"; "complex.h"; "__fc_machdep_linux_gcc_shared.h"]

(* only goes down one level, which is enough for the libc *)
let collect_headers libc_dir =
  let contents =
    List.map (Filename.concat libc_dir) (Array.to_list (Sys.readdir libc_dir))
  in
  let subdirs = List.filter Sys.is_directory contents in
  let base_headers = List.filter is_header contents in
  let all_headers = List.fold_left (fun acc dir ->
      let contents = Array.to_list (Sys.readdir dir) in
      let headers =
        Extlib.filter_map is_header (Filename.concat dir) contents
      in
      acc @ headers
    ) base_headers subdirs
  in
  let all_headers = List.sort Extlib.compare_ignore_case all_headers in
  let to_skip = blacklist () in
  List.iter (fun header ->
      let header_path = Datatype.Filepath.of_string header in
      if List.mem header_path to_skip then
        Format.printf "skipping %a@." Datatype.Filepath.pretty header_path
      else begin
        Kernel.Files.clear ();
        Dynamic.Parameter.String.set "" header;
        Ast.compute ();
      end
    ) all_headers

let () =
  Db.Main.apply (collect_headers (Filename.concat (Sys.getenv "FRAMAC_SHARE") "libc"))
