
type 'a set =
  | Set of 'a list;;

type expr =
  | Var of string
  | Con of int
  | Neg of expr
  | Plus of expr* expr;;

let add x s = match s with | Set l -> Set (x :: l);;

let empty = Set [];;

let union s1 s2 =
  match s2 with
  | Set x2s -> List.fold_left (fun s  -> fun x  -> add x s) s1 x2s;;

let rec free e =
  match e with
  | Var x -> add x empty
  | Const n -> empty
  | Bin (e1,op,e2) -> let f1 = free e1 in let f2 = free e2 in union f1 f2
  | App (e1,e2) -> let f1 = free e1 in let f2 = free e2 in union f1 f2
  | Let (x,e1,e2) ->
      let f1 = free e1 in let f2 = free e2 in union f1 (delete x e2)
  | Fun (x,e1) -> delete x (free e1);;
