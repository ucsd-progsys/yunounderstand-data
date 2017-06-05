
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
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine  -> ("sin(pi*" + exprToString) + ")"
  | Cosine  -> ("cos(pi*" + exprToString) + ")"
  | Average  -> ((("((" + exprToString) + "+") + exprToString) + ")/2"
  | Times  -> (exprToString + "*") + exprToString
  | Thresh  ->
      ((((("(" + exprToString) + "?") + exprToString) + ":") + exprToString)
        + ")"
  | _ -> 0;;
