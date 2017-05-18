
type binop =
  | Plus;;

type expr =
  | Const of int
  | Var of string
  | Bin of expr* binop* expr
  | Let of string* expr* expr
  | App of expr* expr
  | Fun of string* expr;;

let e3' =
  App
    ((Let ("z", (Const 10), (Fun ("y", (Plus ((Var "y"), Plus, (Var "z"))))))),
      (Var "z"));;
