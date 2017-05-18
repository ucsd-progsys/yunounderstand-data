
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
    (VarXarY, VarX, (Times ((Sine VarX), (Cosine (erage (VarX, VarY))))));;
