
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | SixtyNine of expr* expr
  | TheThing of expr* expr* expr;;

let buildSixtyNine e1 = SixtyNine e1;;
