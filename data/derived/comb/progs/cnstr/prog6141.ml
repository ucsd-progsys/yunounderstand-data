
type binop =
  | Plus;;

type expr =
  | Const of int
  | Var of string
  | Bin of expr* binop* expr
  | Let of string* expr* expr
  | App of expr* expr
  | Fun of string* expr;;

let e3 =
  Let
    ("x", (Const 10), (App (Fun ("y", (Bin ((Var "x"), Plus, (Var "y")))))),
      (Var "x"));;
