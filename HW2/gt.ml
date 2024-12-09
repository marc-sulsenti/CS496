(* Marc Sulsenti
   May 30th 2024
   Homework Two
   I pledge my honor that I have abided by the Stevens Honor System  *)

(** Given information from hw2.pdf*)
type 'a gt = Node of 'a * ('a gt list)

let t : int gt = Node (33, [Node (12, []); Node (77, [Node (37, [Node (14, [])]);Node (48, []);Node (103, [])])])

(* Test tree with 1 node (root) *)
let st = Node (1, [])

let mk_leaf : 'a -> 'a gt = fun n -> Node (n, [])

(** [height] is a function that when given a general tree returns the height of that tree.
    Longest path from root to a leaf *)
let rec height t =
  match t with
  | Node (_, []) -> 1  
  | Node (_, children) -> 1 + List.fold_left max 0 (List.map height children)

(** [size] is a function that returns an integer value representing the number of nodes in the tree
  We can assume that the lowest possible value is always 1, because a gt is never empty *)
let rec size t =
  match t with
  | Node (_, children) -> 1 + List.fold_left (fun a children -> a + size children) 0 children

(** [paths_to_leaves] returns a list with all the paths from the root to the leaves of the
general tree t. *)
(* Helper function to add an index to each path in a list of paths *)
let rec paths_to_leaves t =
  match t with
  | Node (_, []) -> [[]]  
  | Node (_, children) ->
  
    let rec paths_from_children index children =
      match children with
      | [] -> []
      | child :: rest ->
        let sub_paths = paths_to_leaves child in
        let checked = 
          let rec add_path index paths =
            match paths with
            | [] -> []
            | path :: rest -> (index :: path) :: add_path index rest
          in
          add_path index sub_paths
        in
        checked @ paths_from_children (index + 1) rest
    in
    paths_from_children 0 children

(** [is_leaf_perfect] determines whether a general tree is leaf perfect. A general tree
    is said to be leaf perfect if all leaves have the same depth *)
let rec depths_finder (Node (_, children)) depth =
  match children with
  | [] -> [depth]
  | _ -> List.flatten (List.map (fun children -> depths_finder children (depth + 1)) children)

let rec is_leaf_perfect t =
  let depth_lists = depths_finder t 0 in
  List.for_all ((=) (List.hd depth_lists)) depth_lists

(** [preorder] preorder traversal is root -> left -> right. Print out the list of the gt in the preorder *)
(*conviently, the list is already given in this order so just go through the list*)
let rec preorder t =
  match t with  
  | Node (data, children) -> data :: (List.flatten (List.map preorder children))

(** [mirror] returns the mirror image of the gt *)
let rec mirror t =
  match t with
  | Node (data, children) -> Node (data, List.map mirror (List.rev children))

(** [map] produces a general tree resulting from t by mapping function f to each data item in t *)
let rec map f t = 
  match t with
  | Node (data, children) -> Node (f data, List.map (map f) children)

(** [fold] encodes a recursion scheme over general trees *)
let rec fold f t =
  match t with
  | Node (data, children) -> f data (List.map (fold f) children)

(** [mirror'] returns the mirror image of gt utilizing fold *)
let mirror' t =
  fold (fun data children -> Node (data, List.rev children)) t

(** [degree] returns the maximum number of children that a node in the tree has *)
let rec degree t =
  match t with
  | Node (_, children) -> max (List.length children) (List.fold_left max 0 (List.map degree children))
