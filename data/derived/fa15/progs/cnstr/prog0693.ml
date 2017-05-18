
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
  | VarX  -> "e"
  | VarY  -> "e"
  | Sine e -> "sin(pi*" ^ ((exprToString e) ^ ")")
  | Cosine e -> "cos(pi*" ^ ((exprToString e) ^ ")")
  | Average e ->
      "((" ^ ((exprToString e) ^ ("+" ^ ((exprToString e) ^ ")/2)")))
  | Times e -> (exprToString e) ^ ("" ^ (exprToString e))
  | Thresh e ->
      "(" ^
        ((exprToString e) ^
           ("<" ^
              ((exprToString e) ^
                 (" ? " ^
                    ((exprToString e) ^ (" : " ^ ((exprToString e) ^ ")")))))));;
