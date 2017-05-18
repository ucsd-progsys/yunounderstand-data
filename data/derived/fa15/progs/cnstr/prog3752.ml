
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Circ of expr* expr* expr
  | Oscillate of expr;;

let buildCirc (c1,c2) = Circ (c1, c2);;
