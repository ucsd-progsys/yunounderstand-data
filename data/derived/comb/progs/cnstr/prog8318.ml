
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Power of expr* expr
  | TowerNeg of expr* expr* expr;;

let sampleExpr5 = TowerNeg (VarX, VarY, VarZ);;
