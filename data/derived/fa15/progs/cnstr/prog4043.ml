
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let sampleExpr1 =
  Thresh
    (VarX, VarY, VarX,
      (Times ((Sine (Exp VarX)), (Cosine (Average (VarX, VarY))))));;
