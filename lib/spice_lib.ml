(*
* Calculates MD5 hash sums for all files in a given directory.
*)


(* A tree structure, where each node has n children. *)
type 'a tree =
  | Leaf (* TODO: of 'a -> the abs. file path! *)
  | Node of 'a * ('a tree list)


let rec size t =
  match t with
  | Leaf -> 0
  (*| Node (_, l, r) -> 1 + (size l) + (size r)*)
  | Node (_, children) -> 1 + (List.fold_left (fun acc x -> acc + (size x)) 0 children)


(*let rec insert_hash t h =*)
let insert_hash t h =
  print_endline "inserting:" ;
  print_endline h ;
  print_endline "its length is:" ;
  print_endline (string_of_int (String.length h)) ;
  match t with
  | Leaf -> Node(h, [])
  (*| Node(y, l, r) ->*)
  | Node(_, _) ->
      Leaf (* TOOD: for testing purpose only! erase!*)


(* Calculates an MD5 hash sum for a file with the full path f. *)
let calculate_hash_for_file f =
  Digest.to_hex(Digest.file f) ;;


(*let%expect_test "tree size test" =
  let t = Leaf in
  print_int (size t);
  [%expect {| 1 |}]*)


(* TODO: erase! *)
let%expect_test "trivial test" =
  print_int (1 + 2);
  [%expect {| 3 |}]
