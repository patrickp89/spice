(*
* Finds duplicate files in a given directory.
*)

open Spice_lib
open Core
open Result

(* Calculates an MD5 hash sum for a file with the full path f. *)
let calculate_hash_for_file (f: string) : (((string * string), string) result) =
  if (Sys.file_exists f) = `No
  then Error ("File " ^ f ^ "does not exist!")
  else begin
    try
      let h = Md5.to_hex (Md5.digest_file_blocking f) in
      printf "md5sum(%s): %s\n" f h;
      Ok (f, h)
    with
    | e -> Error (Exn.to_string e)
  end


(* This function is just a workaround! Travis CI's newest Ubuntu environment is an 18.x LTS distro that ships (highly) outdated
 versions of OCaml itself and of Jane Street's Dune/Base/Core libraries. Sadly, the old Base library does not feature 'List.partition_result'.
 TODO: erase, once Travis has a new Ubuntu (20.x?) LTS version that ships with Base>=0.12 ! *)
let unwrap_result res =
    match res with
    | Ok v -> `Fst v
    | Error e -> `Snd e


(* Computes a Spice tree from all files and folders in directory d. *)
let rec compute_tree_from_files_in_dir r d =
  if (Sys.is_directory d) = `Yes
  then begin
    printf "Entering %s...\n" d;
    try
      let files_in_dir = Sys.ls_dir d in
      let absolute_files_in_dir = List.rev_map files_in_dir ~f:(fun file -> (Filename.concat d file)) in
      let dirs_in_dir, files_that_are_no_dirs = List.partition_tf absolute_files_in_dir ~f:(fun file -> ((Sys.is_directory file) = `Yes)) in
      let file_and_hash_results = List.rev_map files_that_are_no_dirs ~f:(fun file -> calculate_hash_for_file file) in
      (* TODO: The next line is a workaround! Erase once a Base version containing 'List.partition_result' is available via Travis' Ubuntu stack! *)
      let files_and_hashes, errors = List.partition_map file_and_hash_results ~f:(fun res -> (unwrap_result res)) in
      (* TODO: Use "let files_and_hashes, errors = List.partition_result file_and_hash_results in" instead! *)
      List.iter errors ~f:(fun e -> printf "An error occurred: %s\n" e);
      (* create the tree for all files in this directory: *)
      let t = List.fold files_and_hashes ~init:r ~f:(fun acc (file, hash) -> (insert_hash acc hash file)) in
      (* descent into all subdirectores: *)
      List.fold dirs_in_dir ~init:t ~f:(fun acc dir -> (compute_tree_from_files_in_dir acc dir))
    with
    | e -> begin
      printf "Could not read %s: %s \n" d (Exn.to_string e);
    r
    end
  end
  else begin
    printf "%s is not a directory!\n" d;
    r
  end


(* Ask the user how to handle duplicate files. Asks for one duplicate at a time. *)
let manage_duplicatefiles dups =
  printf "I've found %d duplicates!\n" (List.length dups) (* TODO: erase! *)
  (* TODO: prompt the user for a choice (delete vs link vs ...):
  List.iter dups ~f:(fun dup -> printf "Should I (a) erase or (b) create symlinks for %s ?\n" dup) *)


(* Main method. *)
let () =
  if (Array.length Sys.argv) < 2
  then printf "Error! Not enough arguments provided!\n"
  else begin
    let dir = Sys.argv.(1) in
    if not((Sys.file_exists dir) = `Yes) || not((Sys.is_directory dir) = `Yes)
    then printf "Error! '%s' is not a directory or does not exist!\n" dir
    else begin
      let spice_tree = compute_tree_from_files_in_dir createNewSpicyTree dir in
      printf "The resulting tree has %d node(s)!\n" (tree_size spice_tree);
      let duplicates = identify_duplicate_files spice_tree in
      manage_duplicatefiles duplicates
    end
  end
