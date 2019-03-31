(*
* Calculates MD5 hash sums for all files in a given directory.
*)

(* Logs the concatenated string (u +...+ v) to stdout. *)
let log args =
  print_endline (String.concat "" args) ;;

(* Reads all files and folders in d. *)
let files_in_dir d =
  Sys.readdir d ;;

(* Concatenates a directory name d and a filename f. *)
let full_path d f =
  String.concat "" [d; f] ;;

(* Calculates an MD5 hash sum for a file with the full path f. *)
let calculate_hash_for_file f =
  Digest.to_hex(Digest.file f) ;;

(* Prints the MD5 hash sum for file f in directory d. *)
let handle_file d f =
  let p = full_path d f in
  log ["md5sum("; p; ")"] ;
  let h = calculate_hash_for_file p in
  log [f; h] ;;

(* Prints all files and folders in d. *)
let print_files_in_dir d =
  log [d; ":"] ;
  Array.iter (fun f -> handle_file d f) (files_in_dir d) ;;

(* Main method. *)
let () =
  let dir = "/tmp/" in
  print_files_in_dir dir ;;
