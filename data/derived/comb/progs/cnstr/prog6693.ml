
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Mean3 of expr* expr* expr
  | Square of expr;;

let sampleExpr1 =
  Thresh
    (VarX, VarY, VarX,
      (Times
         ((Sine (Mean (VarX, VarX, VarY))),
           (Cosine (Average ((Square VarX), VarY))))));;
