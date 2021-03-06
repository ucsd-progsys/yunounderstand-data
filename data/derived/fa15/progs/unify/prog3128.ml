
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let pi = 4.0 *. (atan 1.0);;

let rec eval (e,x,y) =
  match e with
  | VarX  -> x
  | VarY  -> y
  | Sine e1 -> sin (pi *. (eval e1))
  | Cosine e1 -> cos (pi *. (eval e1))
  | Average (e1,e2) -> (e1 +. e2) / 2
  | Times (e1,e2) -> (eval e1) *. (eval e2)
  | Thresh (e1,e2,e3,e4) -> (e1 < (e2 ?e3) : e4);;
