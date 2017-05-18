
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Hello1 of expr* expr* expr
  | Thresh of expr* expr* expr* expr
  | Hello2 of expr* expr* expr* expr;;

let sampleExpr4 = Hello2 (VarX, VarY, VarX, SinX);;
