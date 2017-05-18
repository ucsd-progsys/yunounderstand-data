
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
  | Sine v -> "sin(pi*" ^ ((exprToString v) ^ ")")
  | Cosine v -> "cos(pi*" ^ ((exprToString v) ^ ")")
  | Average v ->
      "((" ^ ((exprToString v) ^ ("+" ^ ((exprToString v) ^ ")/2)")))
  | Times v -> (exprToString v) ^ ("*" ^ (exprToString v))
  | Thresh v ->
      "(" ^
        ((exprToString v) ^
           ("<" ^
              ((exprToString v) ^
                 ("?" ^ ((exprToString v) ^ (":" ^ ((exprToString v) ^ ")")))))));;
