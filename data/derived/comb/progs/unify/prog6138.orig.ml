type 'a set = Set of 'a list

let empty = Set []

let rec member x s = match s with
  | Set [] -> false
  | Set l -> let (h::tl) = l in if(h = x) then true else member x (Set tl)


let add x s = match s with
  | Set l -> Set (x::l)

let union s1 s2 = match s2 with
  | Set x2s -> List.fold_left (fun s x -> add x s) s1 x2s

let del x s = match s with
  | Set l -> Set(List.filter (fun z -> z!=x) l)


type binop = Plus
type expr = Const of int
          | Var of string
          | Bin of expr * binop * expr
          | Let of string * expr * expr
          | App of expr * expr
          | Fun of string * expr

let rec free e = match e with
  | Var x            -> add x empty
  | Const n          -> empty
  | Bin (e1, op, e2) ->
      (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
      let f1 = free e1 in
      let f2 = free e2 in
        union f1 f2
  | App (e1, e2)     ->
      (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
      let f1 = free e1 in
      let f2 = free e2 in
        union f1 f2
  | Let (x, e1, e2)  ->
      (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
      let f1 = free e1 in
      let f2 = free e2 in
        union f1 (del x e2)
  | Fun (x, e1)      ->
      (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
      del x (free e1)
