
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
  | VarX  -> "%s" e
  | VarY  -> "%s" e
  | Sine  -> "%s" e
  | Cosine  -> "%s" e
  | Average  -> "%s" e
  | Times  -> "%s" e
  | Thresh  -> "%s" e;;
