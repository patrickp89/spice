(*
* Calculates MD5 hash sums for all files in a given directory.
*)


(* A tree structure, where each node has n children. *)
type 'a tree =
  | Leaf (* TODO: of 'b -> the abs. file path! *)
  | Node of 'a * ('a tree list)


(* Creates a new Spice tree. *)
let createNewSpicyTree =
  Node("", [])


(* Calculates how many nodes are in a given Spice tree. *)
let rec size t =
  match t with
  | Leaf -> 1
  | Node (_, children) -> 1 + (List.fold_left (fun acc x -> acc + (size x)) 0 children)


(* Checks, whether the value of a node equals x. *)
let node_value_matches_x t x =
  match t with
  | Leaf -> false
  | Node(v, _) -> ((compare v x) = 0)


(* Checks, whether a node has a child with value x. *)
let exists_child_with_value_x t x =
  match t with
  | Leaf -> false
  | Node(_ , children) -> (List.exists (fun c -> node_value_matches_x c x) children)


let find_child_with_matching_value t x =
  match t with
  | Leaf -> None
  | Node(_ , children) -> begin
    let children_with_matching_value = (List.filter (fun c -> node_value_matches_x c x) children) in
    (*let l = string_of_int (List.length children_with_matching_value) in*)
    (* TODO: if length of 'children_with_matching_value' is > 1, raise an exception! *)
    Some (List.hd children_with_matching_value)
    end


(* A custom exception: to be used when a node that should be present was not found. *)
exception NodeNotFoundException of string (* TODO: erase, once ResultType-refactoring is done! *)

(* A custom exception: to be used when a child node that was identified earlier is not presend. *)
exception NodeHasNoChildren of string (* TODO: erase, once ResultType-refactoring is done! *)


(* Inserts a new hash h into a given Spice tree t. *)
let rec insert_hash t h =
  print_endline ("inserting: " ^ h) ;
  if (String.length h) > 0 then begin
    let h2 = String.sub h 1 ((String.length h) - 1) in
    let c0 = String.sub h 0 1 in
    match t with
    (* if the node is a leaf, but we have a hash character to insert, we simply create a new node: *)
    | Leaf -> Node(c0, [ (insert_hash Leaf h2) ])
    (* otherwise, search its children for a matching child: *)
    | Node(v , children) -> begin
      match (exists_child_with_value_x t c0) with
      (* there is a node with value hash[0] among the children: *)
      | true -> begin
        match (find_child_with_matching_value t c0) with
        | None -> raise (NodeHasNoChildren "This node was a leaf: this should not be possible here!") (* TODO: use a result type instead! *)
        | Some q -> begin
          print_endline ("  Found a child with value '" ^ c0 ^ "'"); (* TODO: erase! *)
          let newQ = insert_hash q h2 in
          let newChildren = List.map (fun c -> if c = q then newQ else c) children in
          Node(v, newChildren)
          end
        end
      (* there is no node with the char, create one: *)
      | false -> begin
        print_endline ("  I didn't find a child with value '" ^ c0 ^ "'!") ; (* TODO: erase! *)
        let newChildren = Node(c0, [ (insert_hash Leaf h2) ]) :: children in
        Node(v, newChildren)
        end
      end
  end
  else begin
  (* TODO: a file name as a leaf! *)
  t
  end;;


(* Calculates an MD5 hash sum for a file with the full path f. *)
let calculate_hash_for_file f =
  Digest.to_hex (Digest.file f)


(* Traverses the Spice tree and identifies duplicate files. *)
let identify_duplicate_files t =
  [ (string_of_int (size t)) ] (* TODO: this is just a dummy placeholder! traverse the tree instead and find all duplicates! *)


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


let%expect_test "node does not have a child with a certain value" =
  let t = Node("parent",
    [ Node("abc", [ Node("def", [ Leaf ]) ]);
      Node("ghi", [ Leaf ]) ]) in
  print_string (string_of_bool (exists_child_with_value_x t "xyz")) ;
  [%expect {| false |}]


let%expect_test "node has a child with a certain value" =
  let t = Node("parent", [ Node("abc", []); Node("ghi", []) ]) in
  print_string (string_of_bool (exists_child_with_value_x t "abc")) ;
  [%expect {| true |}]


let%expect_test "node does not have a child with a certain value (but a child has)" =
  let t = Node("parent",
    [ Node("abc", [ Node("def", [ Leaf ]) ]);
      Node("ghi", [ Leaf ]) ]) in
  print_string (string_of_bool (exists_child_with_value_x t "def")) ;
  [%expect {| false |}]


let%expect_test "a leaf does not contain a certain value" =
  let t = Leaf in
  print_string (string_of_bool (exists_child_with_value_x t "def")) ;
  [%expect {| false |}]


let%expect_test "insert a single hash" =
  let r = createNewSpicyTree in
  let h = "368886bdc82fff1d6f8376b482ac9666" in
  let t = insert_hash r h in
  print_int (size t) ; (* TODO: this test fails due to the many println's! how to circumvent this?? *)
  [%expect {| 34 |}]
