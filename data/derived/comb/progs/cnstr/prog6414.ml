
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Uncreative of expr* expr* expr
  | Creative of expr;;

let sampleExpr5 =
  Uncreative
    (Creative
       (Thresh
          (VarX, VarY, VarX,
            (Times ((Sine VarX), (Cosine (Average (VarX, VarY))))))));;
