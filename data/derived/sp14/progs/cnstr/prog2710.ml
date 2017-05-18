
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | MyExpr1 of expr* expr* expr
  | MyExpr2 of expr;;

let sampleExpr1 =
  MyExpr2
    (MyExpr1
       (Varx, VarY,
         (Thresh
            (VarX, VarY, VarX,
              (Times ((Sine VarX), (Cosine (Average (VarX, VarY)))))))));;
