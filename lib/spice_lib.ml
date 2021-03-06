(*
* Calculates MD5 hash sums for all files in a given directory.
*)

open Core

(* A tree structure, where each node has n children. *)
type ('a, 'b) spice_tree =
  | Leaf of 'b
  | Node of 'a * (('a, 'b) spice_tree list)


(* Creates a new Spice tree. *)
let createNewSpicyTree =
  Node("", [])


(* Calculates how many nodes are in a given Spice tree. *)
let rec tree_size t =
  match t with
  | Leaf _ -> 1
  | Node (_, children) -> 1 + (List.fold children ~init:0 ~f:(fun acc x -> acc + (tree_size x)))


(* Checks, whether the value of a node equals x. *)
let node_value_matches_x (t: (string, string) spice_tree) (x: string) : bool = (* TODO: make the type annotation generic! *)
  match t with
  | Leaf _ -> false
  | Node(v, _) -> (v = x)


(* Checks, whether a node has a child with value x. *)
let exists_child_with_value_x t x =
  match t with
  | Leaf _ -> false
  | Node(_, children) -> (List.exists children ~f:(fun c -> node_value_matches_x c x))


(* Searches among all of t's children, whether one with the given value x exists. Returns that child or None. *)
let find_child_with_matching_value (t: (string, string) spice_tree) (x: string) : (string, string) spice_tree option =
  match t with
  | Leaf _ -> None
  | Node(_, children) -> begin
    match (exists_child_with_value_x t x) with (* TODO: check (List.length children)>0 and (...filter)>0 instead, if List.length is O(1)! *)
    | true -> begin
      let children_with_matching_value = (List.filter children ~f:(fun c -> node_value_matches_x c x)) in
      (*let l2 = string_of_int (List.length children_with_matching_value) in*)
      (* TODO: if length of 'children_with_matching_value' is > 1, raise an exception! *)
      List.hd children_with_matching_value
    end
    | false -> None
  end


(* Inserts a new hash h into a given Spice tree t. *)
let rec insert_hash (t: (string, string) spice_tree) (h: string) (f: string) : (string, string) spice_tree =
  if (String.length h) > 0 then begin
    let h2 = String.sub h ~pos:1 ~len:((String.length h) - 1) in
    let c0 = String.sub h ~pos:0 ~len:1 in
    match t with
    (* all hashes have the same length (i.e. 32 characters), therefore we should never encounter a
    leaf, if String.length(h) is still greater than 0! *)
    | Leaf _ -> begin
      (* TODO: this case should never occurr => raise an exception / return Result.failure ! *)
      printf "I encountered a Leaf where a Node should be!\n"; (* TODO: erase! *)
      t
    end
    (* search the node's children for a matching child: *)
    | Node(v, children) -> begin
      match (find_child_with_matching_value t c0) with
      (* there is no node with the char, create one: *)
      | None ->  begin
        (*print_endline ("  I didn't find a child with value '" ^ c0 ^ "'!") ; TODO: erase! *)
        let newChildren = (insert_hash (Node(c0, [])) h2 f) :: children in
        Node(v, newChildren)
      end
      (* there is a node with value hash[0] among the children: *)
      | Some matchingChild -> begin
        let mcwih = insert_hash matchingChild h2 f in
        let newChildren = List.rev_map children ~f:(fun c -> if c = matchingChild then mcwih else c) in
        Node(v, newChildren)
      end
    end
  end
  else begin
    (* The (partial) hash length is 0, we now insert the file name as a leaf! *)
    match t with
    | Leaf _ -> begin
      (* TODO: this case should never occurr => raise an exception / return Result.failure ! *)
      printf "I visited a Leaf where a Node should be!\n"; (* TODO: erase! *)
      t
    end
    | Node(v, children) -> begin
      (*print_endline ("  Creating a new leaf, node value is " ^ v ^ ", f is " ^ f) ; TODO: erase! *)
      let newChildren = (Leaf(f)) :: children in
      Node(v, newChildren)
    end
  end


(* Checks whether a given Spice tree is a leaf. *)
let is_leaf t =
  match t with
  | Leaf _ -> true
  | Node (_, _) -> false


(* Checks whether the children of a node are leaves. *)
let children_are_leaves trees =
  let bools = (List.rev_map trees ~f:(fun c -> (is_leaf c))) in
  let falses = (List.filter bools ~f:(fun b -> b = false )) in
  (List.length falses) = 0
  (* TODO: use List.for_all instead if map+filter! *)
  (* TODO: use pipe operator! *)


(* Prints the content of a Spice tree. *)
  let node_to_string t =
  match t with
  | Leaf x -> x
  | Node (_, _) -> ""


(* Traverses the given Spice tree and identifies duplicate files. *)
let rec identify_duplicate_files (t: (string, string) spice_tree) : string list =
  match t with
  | Leaf _ -> [ ]
  | Node (x, children) -> begin
    if (children_are_leaves children) then begin
      if ((List.length children) > 1) then begin
        printf "I'm node '... %s' and I have duplicate children:\n" x;
      let leaves_as_string = (List.rev_map children ~f:(fun c -> (node_to_string c))) in
      (List.iter leaves_as_string ~f:(fun c -> (printf " -> %s\n" c)));
      leaves_as_string
      end
      else begin
        []
      end
    end
    else begin
      (List.fold children ~init:[] ~f:(fun acc x -> (List.rev_append (identify_duplicate_files x) acc)))
    end
  end


(* Tests: *)
let%expect_test "tree size test with leaf only" =
  let t = Leaf("a") in
  Out_channel.output_string stdout (Int.to_string ((tree_size t)));
  [%expect {| 1 |}]


let%expect_test "tree size test with single node" =
  let t = Node("parent", [ Node("child one", []); Node("child two", []) ]) in
  Out_channel.output_string stdout (Int.to_string ((tree_size t)));
  [%expect {| 3 |}]


let%expect_test "tree size test with node and leaves" =
  let t = Node("root", [ Leaf("a"); Leaf("b"); Leaf("c"); Leaf("d") ]) in
  Out_channel.output_string stdout (Int.to_string ((tree_size t)));
  [%expect {| 5 |}]


let%expect_test "tree size test with multiple layers of nodes and leaves" =
  let t = Node("parent",
    [ Node("child one", [ Node("grandchild", [ Leaf("a") ]) ]);
      Node("child two", [ Leaf("b") ]) ]) in
  Out_channel.output_string stdout (Int.to_string ((tree_size t)));
  [%expect {| 6 |}]


let%expect_test "node does not have a child with a certain value" =
  let t = Node("parent",
    [ Node("abc", [ Node("def", [ Leaf("a") ]) ]);
      Node("ghi", [ Leaf("b") ]) ]) in
  print_string (string_of_bool (exists_child_with_value_x t "xyz")) ;
  [%expect {| false |}]


let%expect_test "node has a child with a certain value" =
  let t = Node("parent", [ Node("abc", []); Node("ghi", []) ]) in
  Out_channel.output_string stdout (string_of_bool (exists_child_with_value_x t "abc"));
  [%expect {| true |}]


let%expect_test "node does not have a child with a certain value (but a child has)" =
  let t = Node("parent",
    [ Node("abc", [ Node("def", [ Leaf("a") ]) ]);
      Node("ghi", [ Leaf("b") ]) ]) in
  Out_channel.output_string stdout (string_of_bool (exists_child_with_value_x t "def")) ;
  [%expect {| false |}]


let%expect_test "a leaf does not contain a certain value" =
  let t = Leaf("a") in
  Out_channel.output_string stdout (string_of_bool (exists_child_with_value_x t "def")) ;
  [%expect {| false |}]


let%test "insert a single hash" =
  let r = createNewSpicyTree in
  let h = "368886bdc82fff1d6f8376b482ac9678" in
  let t = insert_hash r h "filename.ext" in
  tree_size t = 34 (* 34 = 1 root + 32 nodes + 1 leaf *)


let%test "insert hash twice" =
  let r = createNewSpicyTree in
  let h = "368886bdc82fff1d6f8376b482ac9666" in
  let t = insert_hash r h "filename.ext" in
  let t2 = insert_hash t h "second.file" in
  tree_size t2 = 35


let%test "identify duplicates" =
  let r = createNewSpicyTree in
  let h = "368886bdc82fff1d6f8376b482ac9666" in
  let t = insert_hash r h "filename.ext" in
  let t2 = insert_hash t h "second.file" in
  let dups = identify_duplicate_files t2 in
  List.length dups = 2

let%test "no duplicates identified" =
  let r = createNewSpicyTree in
  let h = "368886bdc82fff1d6f8376b482ac9699" in
  let t = insert_hash r h "filename.3xt" in
  let dups = identify_duplicate_files t in
  List.length dups = 0
