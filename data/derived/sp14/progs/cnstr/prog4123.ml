
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
  | Sine e -> "sin (pi*" ^ ((exprToString e) ^ ")")
  | Cosine e -> "cos (pi*" ^ ((exprToString e) ^ ")")
  | Averages  ->
      "((" ^ ((exprToString e) ^ ("*" ^ ((exprToString e) ^ ")/2)")))
  | Times  -> "(" ^ ((exprToString e) ^ ("*" ^ ((exprToString e) ^ ")")))
  | Thresh  ->
      "(" ^
        ((exprToString e) ^
           ("<" ^
              ((exprToString e) ^
                 ("?" ^ ((exprToString e) ^ (":" ^ ((exprToString e) ^ ")")))))));;
