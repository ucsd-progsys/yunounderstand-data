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


let _ = lookup "a" [("a", 1); ("b", 2); ("a", 10)]

          let _ = lookup "z" [("a", 1); ("b", 2); ("a", 10)]

  let lift1 f xo = match xo with
  | None -> None
                | Some x -> Some (f x)

          let _ = lift1  string_of_int None

  let lift2 f xo yo = match (xo,yo) with
  | (Some x, Some y) -> Some (f x y)
    | (_,_) -> None

          let _ = lift2 (+) (Some 1) (Some 10)

let _ = lift2 (+) (Some 1) (None);;



          let _ = lift2 (-) (Some 0) (Some 2)


          type expr = Var of string (*XXXXXXXXXX*)
          | Con of int (*XXXXXXXXXX*)
          | Neg of expr (*XXXXXXXXXXXXXXXXXXXXXXXXXXX*)
                    | Plus of expr * expr (*XXXXXXXXXXXXXXXXXXXXXXXX*)

  let rec eval env e =  match e with
  | Var x -> lookup x env
  | Con i -> Some i
| Neg e' -> lift2 (-) (Some 0) (eval env e')
| Plus (e1,e2) -> lift2 (+) (eval env e1) (eval env e2)