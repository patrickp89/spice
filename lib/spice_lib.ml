(*
* Calculates MD5 hash sums for all files in a given directory.
*)


(* A tree structure, where each node has n children. *)
type 'a tree =
  | Leaf (* TODO: of 'a -> the abs. file path! *)
  | Node of 'a * ('a tree list)


let createNewSpicyTree =
  Leaf (* TODO: Node("", []) instead! *)


let rec size t =
  match t with
  | Leaf -> 1
  | Node (_, children) -> 1 + (List.fold_left (fun acc x -> acc + (size x)) 0 children)


let does_node_has_child_with_value_x (Node(_ , children)) x =
  let l = List.length children in
  print_endline ("this node has" ^ (string_of_int l) ^ "nodes!") ;
  print_endline ("searching for value " ^ x ^ "...") ;
  true


(*let rec insert_hash t h =*)
let insert_hash t h =
  print_endline ("inserting: " ^ h) ;
  print_endline "its length is:" ;
  print_endline (string_of_int (String.length h)) ;
  if (String.length h) > 0 then begin
    let h2 = String.sub h 1 ((String.length h) - 1) in
    print_endline "h2 is:" ;
    print_endline h2 ;
    let c0 = String.sub h 0 1 in
    print_endline "h's first char is:" ;
    print_endline c0 ;
    match t with
    | Leaf -> Node(c0, [])
    (*| Node(y, l, r) ->*)
    | Node(_, _(*children*)) -> Leaf (*begin
      (*let opt = List.find_opt (fun x -> if x = c0 then true else false) children in TODO: use 'exists' instead??*)
      let opt = List.find_opt (fun Node(x, _) -> (compare x c0) = 0) children in (* TODO: use 'exists' instead??*)
      match opt with
      (* there is a node with value hash[0] among the children: *)
      | Some c -> insert_hash childX h2 (* TODO: find the child!*)
      (* there is no node with the char, create one: *)
      | None -> (* TODO: ...*)
      Leaf (* TOOD: for testing purpose only! erase!*)
      (*if x = y then t (* TODO: ... *)
      else if x < y then Node(y, (insert l x), r)
      else Node(y, l, (insert r x))*)
      end ;*)
  end else t ;;


(* Calculates an MD5 hash sum for a file with the full path f. *)
let calculate_hash_for_file f =
  Digest.to_hex(Digest.file f) ;;


(* Tests: *)
let%expect_test "tree size test with leaf only" =
  let t = Leaf in
  print_int (size t) ;
  [%expect {| 1 |}]


let%expect_test "tree size test with single node" =
  let t = Node("parent", [ Node("child one", []); Node("child two", []) ]) in
  print_int (size t) ;
  [%expect {| 3 |}]


let%expect_test "tree size test with node and leaves" =
  let t = Node("root", [ Leaf; Leaf; Leaf; Leaf ]) in
  print_int (size t) ;
  [%expect {| 5 |}]


let%expect_test "tree size test with multiple layers of nodes and leaves" =
  let t = Node("parent",
    [ Node("child one", [ Node("grandchild", [ Leaf ]) ]);
      Node("child two", [ Leaf ]) ]) in
  print_int (size t) ;
  [%expect {| 6 |}]


let%expect_test "node contains child with certain value" =
  let t = Node("parent",
    [ Node("abc", [ Node("def", [ Leaf ]) ]);
      Node("ghi", [ Leaf ]) ]) in
  print_string (string_of_bool (does_node_has_child_with_value_x t "xyz")) ;
  [%expect {| false |}]
