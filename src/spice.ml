(*
* Calculates MD5 hash sums for all files in a given directory.
*)

(*open Base
open Stdio*)


type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree ;;


(* TODO: the insert function must insert the _hash_ _lexicographically_! *)
let rec insert t x =
  match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node(y, l, r) ->
      if x = y then t
      else if x < y then Node(y, (insert l x), r)
      else Node(y, l, (insert r x))


let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + (size l) + (size r)


(* Logs the concatenated string (u +...+ v) to stdout. *)
let log args =
  print_endline (String.concat "" args) ;;


(* Concatenates a directory name d and a filename f. *)
let full_path d f =
  String.concat "" [d; f] ;;


(* Calculates an MD5 hash sum for a file with the full path f. *)
let calculate_hash_for_file f =
  Digest.to_hex(Digest.file f) ;;


(* Calculates an MD5 hash sum for a given file f and inserts the hash into the given tree t. *)
let calculate_and_insert_hash_into_t f t =
  log ["md5sum("; f; ")"] ;
  let h = calculate_hash_for_file f in
  log [f; h] ;
  (* TODO: insert the hash 'insert t h ;' *)
  log ["Tree has "; string_of_int (size t); " nodes"] ;;


(* Places the MD5 hash sum for file f in directory d into the tree t of MD5 sums. Returns the new tree. *)
let handle_file d f t =
  let p = full_path d f in
  if not (Sys.is_directory p)
  then calculate_and_insert_hash_into_t p t
  else log [p; " is a directory!"] ;; (* TODO; recursive descent! *)


(* Reads all files and folders in d. *)
let files_in_dir d =
  Sys.readdir d ;;


(* Prints all files and folders in d. *)
let print_files_in_dir d =
  log [d; ":"] ;
  let t = Leaf in
  (* TODO: insert a root -> 'insert t d ;' *)
  Array.iter (fun f -> (handle_file d f t)) (files_in_dir d) ;;


(* Main method. *)
let () =
  let dir = "/tmp/" in
  print_files_in_dir dir ;;
