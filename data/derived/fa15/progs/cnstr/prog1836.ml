
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Cube of expr* expr
  | Square of expr;;

let sampleExpr1 =
  Thresh
    (VarX, VarY, VarX,
      (Cube (Times ((Sine VarX), (Cosine (Average (VarX, VarY)))))));;
