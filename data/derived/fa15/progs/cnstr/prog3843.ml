
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Tan of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Tangent of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let sampleExpr1 =
  Thresh
    (VarX, VarY, VarX,
      (Times ((Tan (Sine VarX)), (Cosine (Average (VarX, VarY))))));;
