
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Expn of expr* expr
  | TripMult of expr* expr* expr;;

let buildExpn b = Expn b;;
