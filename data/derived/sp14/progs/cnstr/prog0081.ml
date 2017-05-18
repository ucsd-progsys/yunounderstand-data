
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Square of expr
  | Average of expr* expr
  | Times of expr* expr
  | MyExpr of expr* expr* expr* expr
  | Thresh of expr* expr* expr* expr;;

let buildMyExpr (a,b,a_less) = MyExpr (a, b, a_less);;
