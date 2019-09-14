(*
* Finds duplicate files in a given directory.
*)

open Spice_lib
open Core

(* Concatenates a directory name d and a filename f. *)
let full_path d f =
  Filename.concat d f


(* Calculates an MD5 hash sum for a given file f and inserts the hash into the given tree t. *)
let calculate_and_insert_hash_into_tree f t =
  let h = calculate_hash_for_file f in
  printf "md5sum(%s): %s\n" f h;
  insert_hash t h f


(* Checks, if a file/folder is readable. (Substitutes Sys.file_exists, which does not reliably test for file permissions.) *)
let is_readable f =
  if (Sys.is_directory f) = `Yes
  then try
      (* a dummy call to check whether the directory is accessible: *)
      (Array.length (Sys.readdir f)) >= 0
    with
    | _ -> false
  else try
      (* Is the file readable? *)
      (String.length (Md5.to_hex (Md5.digest_file_blocking f))) > 0 (* TODO: this call is computationally expensive, simplify it! *)
    with
    | _ -> false


(* Computes a Spice tree from all files and folders in directory d. *)
let rec compute_tree_from_files_in_dir r d =
  let files_in_dir = Array.to_list (Sys.readdir d) in
  let absolute_files_in_dir = List.rev_map files_in_dir ~f:(fun file -> (full_path d file)) in
  let readable_files_in_dir = List.filter absolute_files_in_dir ~f:(fun file -> (is_readable file)) in
  let unreadable_files = List.filter absolute_files_in_dir ~f:(fun file -> not(List.mem readable_files_in_dir file ~equal:(fun a b -> a = b))) in
  if (List.length unreadable_files) > 0
  then begin
    printf "I don't have permission to read the following %d file(s):\n" (List.length unreadable_files);
    (List.iter unreadable_files ~f:(fun file -> (printf " ~~> '%s'\n" file)))
  end;
  List.fold readable_files_in_dir ~init:r ~f:(fun acc file -> handle_file file acc)


(* Places the MD5 hash sum of file f in directory d into the Spice tree t. Returns the new tree. *)
and handle_file f t =
  (* check whether we've got read permission: *)
  if (Sys.file_exists f) = `Yes
  then begin
    (* is this a directory or a file? *)
    if (Sys.is_directory f) = `No
    then calculate_and_insert_hash_into_tree f t
    else begin
      printf "Entering %s...\n" f;
      compute_tree_from_files_in_dir t f
    end
  end
  else begin
    printf "Error! I cannot read '%s'!\n" f;
    t
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
