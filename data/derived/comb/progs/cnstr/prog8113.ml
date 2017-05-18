
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Neg of expr
  | Average of expr* expr
  | Times of expr* expr
  | Divadd of expr* expr* expr
  | Thresh of expr* expr* expr* expr;;

type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Neg of expr
  | Average of expr* expr
  | Times of expr* expr
  | AddMul of expr* expr* expr
  | Thresh of expr* expr* expr* expr;;

let sampleExpr3 = Neg (Divadd (VarX, VarY, VarY));;
