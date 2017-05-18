type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
let ans0 = Node (2, Node (1, Leaf, Leaf)
                , Node (3, Leaf, Leaf))

let rec flerb xs = match xs with
  | [] -> Leaf
  | x::xs' -> Node (x, Leaf, flerb xs')


let rec glub f t = match t with
  | Leaf -> Leaf
  | Node (x,l,r) -> Node (f x, glub f l, glub f r)
let ans = glub (fun x -> 2 * x) ans0



type 'a option = None | Some of 'a

let safeDiv num den = match den with
  | 0 -> None
  | _ -> Some (num / den)

let rec lookup k kvs = match kvs with
  | [] -> None
  | (c,v)::tl -> if (c = k) then Some v else lookup k tl


let _ = lookup "a" [("a", 1); ("b", 2), ("a", 10)]
          
let _ = lookup "z" [("a", 1); ("b", 2), ("a", 10)]