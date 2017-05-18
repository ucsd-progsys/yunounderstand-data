
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec exprToString e =
  match e with
  | VarX  -> printf "A"
  | VarY  -> printf "A"
  | Sine  -> printf "A"
  | Cosine  -> printf "A"
  | Average  -> printf "A"
  | Times  -> printf "A"
  | Thresh  -> printf "A";;
