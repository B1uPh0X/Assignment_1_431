(*
Problem 10
Christopher G Mavros
*)
let rec minval lst =
    match lst with
    | [] -> raise (Failure "Empty list")
    | [x] -> x
    | x :: xs -> min x (minval xs);;

let a = [325; 272; 643; 89; 322] ;;


Printf.printf "%d\n" (minval a);;

(*
Problem 11 A & B
Christopher G Mavros
*)

type treeFloat =
    | Empty
    | Node of float * treeFloat * treeFloat

let treeOfFloats = 
    Node(5.2, 
        Node(4.2, 
            Node(6.9, Empty, Empty), 
            Node(1.5, Empty,
                Node(9.6, Empty, Empty)
            )
        ), 
        Node(8.0, 
            Node(4.1, Empty, Empty), 
            Node(15.2, Empty, Empty)
        )
    )

(*
Problem 12
Christopher G Mavros
*)

let rec depth tree =
    match tree with
    | Empty -> 0
    | Node(_, left, right) -> 1 + max (depth left) (depth right)

let fTreeDepth = depth treeOfFloats ;;

Printf.printf "%d\n" (fTreeDepth) ;;

(*
Problem 13
Christopher G Mavros
*)

let rec leafCount tree =
    match tree with
    | Empty -> 0
    | Node (_, Empty, Empty) -> 1
    | Node (_, l, r) -> (leafCount l) + (leafCount r)

let numLeaves = leafCount treeOfFloats ;;

Printf.printf "%d\n" (numLeaves) ;;