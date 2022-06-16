(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg2), (expected_output))
]
*)
let graph1 = {
  nodes = ["A"; "B"; "C"; "D"; "E"; "F"];
  edges = [("A", "C", 3); ("A", "E", 5); 
           ("B", "C", 4); ("B", "D", 2);
           ("C", "D", 1); ("C", "F", 7);
           ("D", "A", 8); ("D", "F", 6); 
           ("F", "E", 19)]
}
(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  ((graph1, "A"),([("C", 3);("E", 5)]));
  ((graph1, "D"),([("A", 8);("F", 6)]));
  ((graph1, "F"),([("E", 19)]));
  ((graph1, "E"),([]))
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list = List.fold_left 
    (fun acc (v1, v2, w) -> if v1=vertex then (v2,w)::acc else acc) [] g.edges

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) = 
  let rec aux_node ((node: 'a) , weight) (visited : 'a list) : ('a list * weight) =
    if node = b then ([b], weight)
    else if List.exists (fun v -> v = node) visited then raise Fail
    else let (p,c) = aux_list (neighbours g node) (node :: visited) 
      in (node::p, c+weight) 
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) =
    match nodes with 
    |[] -> raise Fail
    |(v,w)::vs-> try aux_node (v,w) visited with Fail -> aux_list vs visited 
  in
  aux_node (a,0) []

(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node ((node: 'a) , weight) (visited : 'a list) fc sc : ('a list * weight)=
    if node = b then sc ([b], weight)
    else if List.exists (fun v -> v = node) visited then fc ()
    else aux_list (neighbours g node) (node::visited) fc 
        (fun (p,c)->sc (node::p, c+weight))
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) =
    match nodes with
    |[] -> fc ()
    |(v,w)::vs->aux_node (v,w) visited (fun ()->aux_list vs visited fc sc) sc
  in
  aux_node (a,0) [] (fun () -> raise Fail) (fun x -> x)

(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list =
  let rec aux_node ((node: 'a) , weight) (visited : 'a list) : ('a list * weight) list =
    if node = b then [([b], weight)]
    else if List.exists (fun v -> v = node) visited then raise Fail
    else List.map (fun (p,c)->(node::p, c+weight))
        (aux_list (neighbours g node) (node :: visited))
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) : ('a list * weight) list =
    match nodes with 
    |[] -> []
    |(v,w)::vs->try aux_node (v,w) visited @ aux_list vs visited
        with Fail -> aux_list vs visited
  in
  aux_node (a,0) []


(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option =
  let rec short pw = match pw with
    | [] -> None
    | [(p,w)] -> Some (p,w)
    | (p1,w1)::(p2,w2)::pws -> if w1<w2 then short((p1,w1)::pws) else short((p2,w2)::pws)
  in
  short (find_all_paths g a b)
