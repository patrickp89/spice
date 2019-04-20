(*
* Finds duplicate files in a given directory.
*)

open Spice_lib


(* Logs the concatenated string (u +...+ v) to stdout. *)
let log args =
  print_endline (String.concat "" args)


(* Concatenates a directory name d and a filename f. *)
let full_path d f =
  String.concat "" [d; f]


(* Calculates an MD5 hash sum for a given file f and inserts the hash into the given tree t. *)
let calculate_and_insert_hash_into_tree f t =
  print_endline " \n" ; (* TODO: erase! *)
  log ["md5sum("; f; ")"] ;
  let h = calculate_hash_for_file f in
  log [f; ": "; h] ;
  let t = insert_hash t h in
  log ["[calcula...] Tree has "; string_of_int (size t); " node(s)"] ;
  t


(* Places the MD5 hash sum of file f in directory d into the Spice tree t. Returns the new tree. *)
let handle_file d f t =
  let p = full_path d f in
  if not (Sys.is_directory p)
  then calculate_and_insert_hash_into_tree p t
  else begin
    log [p; " is a directory!"] ;
    t (* TODO; recursive descent! *)
  end


(* Reads all files and folders in d. *)
let files_in_dir d =
  Sys.readdir d


(* Computes a Spice tree from all files and folders in directory d. *)
let compute_tree_from_files_in_dir d =
  log [d; ":"] ;
  (* TODO: does d exist? does it have a trailing slash? *)
  let r = createNewSpicyTree in
  let t = Array.fold_left (fun acc f -> handle_file d f acc) r (files_in_dir d) in
  log ["[compute...] Tree has "; string_of_int (size t); " node(s)"] ;
  t


(* Ask the user how to handle duplicate files. Asks for one duplicate at a time. *)
let manage_duplicatefiles dups =
  List.iter (fun f -> print_endline f) dups (* TODO: prompt the user for a choice (delete vs link vs ...) *)


(* Main method. *)
let () =
  let dir = "/opt/test/" in (* TODO: get from command line! *)
  let spice_tree = compute_tree_from_files_in_dir dir in
  let duplicates = identify_duplicate_files spice_tree in
  manage_duplicatefiles duplicates
