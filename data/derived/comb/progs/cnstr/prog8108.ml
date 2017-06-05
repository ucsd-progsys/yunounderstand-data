
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Half of expr
  | Neg of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Half of expr
  | Average of expr* expr
  | Times of expr* expr
  | AddDivide of expr* expr* expr
  | Thresh of expr* expr* expr* expr;;

let sampleExpr3 = Neg (AddDivide (VarX, VaryX, VarY));;
