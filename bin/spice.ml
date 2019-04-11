(*
* Finds duplicate files in a given directory.
*)

open Spice_lib


(* Logs the concatenated string (u +...+ v) to stdout. *)
let log args =
  print_endline (String.concat "" args) ;;


(* Concatenates a directory name d and a filename f. *)
let full_path d f =
  String.concat "" [d; f] ;;


(* Calculates an MD5 hash sum for a given file f and inserts the hash into the given tree t. *)
let calculate_and_insert_hash_into_tree f t =
  print_endline " \n" ; (* TODO: erase! *)
  log ["md5sum("; f; ")"] ;
  let h = calculate_hash_for_file f in
  log [f; ": "; h] ;
  let t = insert_hash t h in
  log ["Tree has "; string_of_int (size t); " nodes"] ;
  t ;;


(* Places the MD5 hash sum for file f in directory d into the tree t of MD5 sums. Returns the new tree. *)
let handle_file d f t =
  let p = full_path d f in
  if not (Sys.is_directory p)
  then calculate_and_insert_hash_into_tree p t
  else begin
  log [p; " is a directory!"] ;
  t end ;; (* TODO; recursive descent! *)


(* Reads all files and folders in d. *)
let files_in_dir d =
  Sys.readdir d ;;


(* Computes a Spice tree from all files and folders in directory d. *)
let compute_tree_from_files_in_dir d =
  log [d; ":"] ;
  let t = Leaf in (* TODO: insert an [arbitrary?] root! *)
  (*Array.iter (fun f -> (let t = (handle_file d f t))) (files_in_dir d) ;; *)
  (* TODO: below is only for testing purposes! make the line above work again! *)
  handle_file d (Array.get (files_in_dir d) 0) t ;;


(* Prints a tree... *)
let print_tree t =
  log ["Tree has "; string_of_int (size t); " nodes"] ;; (* TODO: print it! *)


(* Main method. *)
let () =
  let dir = "/tmp/" (* TODO: get from command line... *) in
  let spice_tree = compute_tree_from_files_in_dir dir in
  print_tree spice_tree ;; (* TODO: traverse the tree instead, and print all _duplicates_!*)