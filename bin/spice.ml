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
  logg ["md5sum("; f; ")"] ;
  let h = calculate_hash_for_file f in
  logg [f; ": "; h] ;
  let t = insert_hash t h f in
  logg ["[calcula...] Tree has "; Int.to_string (tree_size t); " node(s)"] ;
  t


(* Computes a Spice tree from all files and folders in directory d. *)
let rec compute_tree_from_files_in_dir r d =
  logg [d; ":"] ;
  let files_in_dir = Array.to_list (Core.Sys.readdir d) in
  let absolute_files_in_dir = List.rev_map files_in_dir ~f:(fun file -> (full_path d file)) in
  let readable_files_in_dir = List.filter absolute_files_in_dir ~f:(fun file -> (Core.Sys.file_exists file) = `Yes) in
  let unreadable_files = List.filter absolute_files_in_dir ~f:(fun file -> not((Core.Sys.file_exists file) = `Yes)) in
  logg ["I don't have permission to read the following "; string_of_int (List.length unreadable_files); " files:"];
  (* TODO: filtering unreadable files does not work! -> create a custom predicate to check for this! *)
  (List.iter unreadable_files ~f:(fun file -> (logg [" ~~> '"; file; "'"])));
  logg ["I'll deal with these "; string_of_int (List.length readable_files_in_dir); " files:"];
  (List.iter readable_files_in_dir ~f:(fun file -> (logg [" --> '"; file; "'"])));
  let t = List.fold readable_files_in_dir ~init:r ~f:(fun acc file -> handle_file file acc) in
  logg ["[compute...] Tree has "; string_of_int (tree_size t); " node(s)"] ;
  t


(* Places the MD5 hash sum of file f in directory d into the Spice tree t. Returns the new tree. *)
and handle_file f t =
  (* check whether we've got read permission: *)
  if (Core.Sys.file_exists f) = `Yes
  then begin
    (* is this a directory or a file? *)
    if (Core.Sys.is_directory f) = `No
    then begin
      logg [ " --> calculating hash for '"; f; "'..." ];
      calculate_and_insert_hash_into_tree f t
    end
    else begin
      logg [ "Entering "; f; "..." ] ;
      compute_tree_from_files_in_dir t f
    end
  end
  else begin
    logg [ "Error! I cannot read '"; f; "'!" ];
    t
  end


(* Ask the user how to handle duplicate files. Asks for one duplicate at a time. *)
let manage_duplicatefiles dups =
  List.iter dups ~f:(fun dup -> print_endline dup) (* TODO: prompt the user for a choice (delete vs link vs ...) *)


(* Main method. *)
let () =
  if (Array.length Sys.argv) < 2
  then logg [ "Error! Not enough arguments provided!" ]
  else begin
    let dir = Sys.argv.(1) in
    if not((Core.Sys.file_exists dir) = `Yes) || not((Core.Sys.is_directory dir) = `Yes)
    then logg [ "Error! '"; dir; "' is not a directory or does not exist!" ]
    else begin
      let spice_tree = compute_tree_from_files_in_dir createNewSpicyTree dir in
      let duplicates = identify_duplicate_files spice_tree in
      manage_duplicatefiles duplicates
    end
  end
