
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Power of expr
  | KellysOp of expr* expr* expr* expr;;

let buildKellysOp (a,b,a_more) = KellysOp (a, b, a_more);;
