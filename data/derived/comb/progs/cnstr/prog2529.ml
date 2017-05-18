
type tree =
  | Leaf of int
  | Node of tree* tree;;

let _ =
  let rec foo t =
    match t with | Leaf n -> 1 | Node (t1,t2) -> (foo t1) + (foo t2) in
  foo (Node ((Node ((Leaf 1), (Leaf 2))), Leaf3));;
