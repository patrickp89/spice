(*
* Calculates MD5 hash sums for all files in a given directory.
*)


(* A tree structure, where each node has n children. *)
type ('a, 'b) spice_tree =
  | Leaf of 'b
  | Node of 'a * (('a, 'b) spice_tree list)


(* Creates a new Spice tree. *)
let createNewSpicyTree =
  Node("", [])


(* Calculates how many nodes are in a given Spice tree. *)
let rec size t =
  match t with
  | Leaf _ -> 1
  | Node (_, children) -> 1 + (List.fold_left (fun acc x -> acc + (size x)) 0 children)


(* Checks, whether the value of a node equals x. *)
let node_value_matches_x t x =
  match t with
  | Leaf _ -> false
  | Node(v, _) -> ((compare v x) = 0)


(* Checks, whether a node has a child with value x. *)
let exists_child_with_value_x t x =
  match t with
  | Leaf _ -> false
  | Node(_ , children) -> (List.exists (fun c -> node_value_matches_x c x) children)


(* Searches among all of t's children, whether one with the given value x exists. Returns that child or None. *)
let find_child_with_matching_value t x =
  match t with
  | Leaf _ -> None
  | Node(_ , children) -> begin
    match (exists_child_with_value_x t x) with (* TODO: check (List.length children)>0 and (...filter)>0 instead, if List.length is O(1)! *)
    | true -> begin
      let children_with_matching_value = (List.filter (fun c -> node_value_matches_x c x) children) in
      (*let l2 = string_of_int (List.length children_with_matching_value) in*)
      (* TODO: if length of 'children_with_matching_value' is > 1, raise an exception! *)
      Some (List.hd children_with_matching_value)
    end
    | false -> None
  end


(* A custom exception: to be used when a node that should be present was not found. *)
exception NodeNotFoundException of string (* TODO: erase, once ResultType-refactoring is done! *)

(* A custom exception: to be used when a child node that was identified earlier is not presend. *)
exception NodeHasNoChildren of string (* TODO: erase, once ResultType-refactoring is done! *)


(* Inserts a new hash h into a given Spice tree t. *)
let rec insert_hash (t: (string, string) spice_tree) (h: string) (f: string) : (string, string) spice_tree =
  if (String.length h) > 0 then begin
    let h2 = String.sub h 1 ((String.length h) - 1) in
    let c0 = String.sub h 0 1 in
    match t with
    (* if the node is a leaf, but we have a hash character to insert, we simply create a new node: *)
    | Leaf _ -> begin
      (* TODO: this case should never occurr => raise an exception! *)
      print_endline ("I visited a Leaf where a Node should be!") ; (* TODO: erase! *)
      t
    end

    (* otherwise, search its children for a matching child: *)
    | Node(v , children) -> begin
      match (find_child_with_matching_value t c0) with
      (* there is no node with the char, create one: *)
      | None ->  begin
        (*print_endline ("  I didn't find a child with value '" ^ c0 ^ "'!") ; TODO: erase! *)
        let newChildren = (insert_hash (Node(c0, [])) h2 f) :: children in
        Node(v, newChildren)
      end
      (* there is a node with value hash[0] among the children: *)
      | Some matchingChild -> begin
        (*print_endline ("  Found a child with value '" ^ c0 ^ "'"); TODO: erase! *)
        let mcwih = insert_hash matchingChild h2 f in
        let newChildren = List.map (fun c -> if c = matchingChild then mcwih else c) children in
        Node(v, newChildren)
      end
    end
  end
  else begin
    (* The (partial) hash length is 0, we now insert the file name: *)
    match t with
    | Leaf _ -> begin
      (* TODO: this case should never occurr => raise an exception! *)
      print_endline ("I visited a Leaf where a Node should be!") ; (* TODO: erase! *)
      t
    end
    | Node(v , children) -> begin
      (*print_endline ("  Creating a new leaf, node value is " ^ v ^ ", f is " ^ f) ; TODO: erase! *)
      let newChildren = (Leaf(f)) :: children in
      Node(v, newChildren)
    end
  end


(* Calculates an MD5 hash sum for a file with the full path f. *)
let calculate_hash_for_file f =
  Digest.to_hex (Digest.file f)


(* Traverses the Spice tree and identifies duplicate files. *)
let identify_duplicate_files t =
  (*match t with
  | Leaf _ -> ...
  | Node (_, children) -> ...*)
  [ (string_of_int (size t)) ] (* TODO: this is just a dummy placeholder! traverse the tree instead and find all duplicates! *)


(* Tests: *)
let%expect_test "tree size test with leaf only" =
  let t = Leaf("a") in
  print_int (size t) ;
  [%expect {| 1 |}]


let%expect_test "tree size test with single node" =
  let t = Node("parent", [ Node("child one", []); Node("child two", []) ]) in
  print_int (size t) ;
  [%expect {| 3 |}]


let%expect_test "tree size test with node and leaves" =
  let t = Node("root", [ Leaf("a"); Leaf("b"); Leaf("c"); Leaf("d") ]) in
  print_int (size t) ;
  [%expect {| 5 |}]


let%expect_test "tree size test with multiple layers of nodes and leaves" =
  let t = Node("parent",
    [ Node("child one", [ Node("grandchild", [ Leaf("a") ]) ]);
      Node("child two", [ Leaf("b") ]) ]) in
  print_int (size t) ;
  [%expect {| 6 |}]


let%expect_test "node does not have a child with a certain value" =
  let t = Node("parent",
    [ Node("abc", [ Node("def", [ Leaf("a") ]) ]);
      Node("ghi", [ Leaf("b") ]) ]) in
  print_string (string_of_bool (exists_child_with_value_x t "xyz")) ;
  [%expect {| false |}]


let%expect_test "node has a child with a certain value" =
  let t = Node("parent", [ Node("abc", []); Node("ghi", []) ]) in
  print_string (string_of_bool (exists_child_with_value_x t "abc")) ;
  [%expect {| true |}]


let%expect_test "node does not have a child with a certain value (but a child has)" =
  let t = Node("parent",
    [ Node("abc", [ Node("def", [ Leaf("a") ]) ]);
      Node("ghi", [ Leaf("b") ]) ]) in
  print_string (string_of_bool (exists_child_with_value_x t "def")) ;
  [%expect {| false |}]


let%expect_test "a leaf does not contain a certain value" =
  let t = Leaf("a") in
  print_string (string_of_bool (exists_child_with_value_x t "def")) ;
  [%expect {| false |}]


let%test "insert a single hash" =
  let r = createNewSpicyTree in
  let h = "368886bdc82fff1d6f8376b482ac9678" in
  let t = insert_hash r h "filename.ext" in
  size t = 34 (* 34 = 1 root + 32 nodes + 1 leaf *) (* TODO: use an expectation test instead? (Get rid of unnecessary output first!) *)


let%test "insert hash twice" =
  let r = createNewSpicyTree in
  let h = "368886bdc82fff1d6f8376b482ac9666" in
  let t = insert_hash r h "filename.ext" in
  let t2 = insert_hash t h "second.file" in
  size t2 = 35 (* TODO: use an expectation test instead? (Get rid of unnecessary output first!) *)
