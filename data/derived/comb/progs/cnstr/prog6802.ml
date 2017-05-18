
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
  | VarX  -> "VarX"
  | VarY  -> "VarY"
  | Sine  -> "Sine(" ^ ((exprToString e) ^ ")")
  | Cosine  -> "Cosine(" ^ ((exprToString e) ^ ")")
  | Average  -> "Average(" ^ ((exprToString e) ^ ")")
  | Times  -> "Times(" ^ ((exprToString e) ^ ")")
  | Thresh (a,b,c,d) ->
      "Thresh(" ^
        ((exprToString a) ^
           ("," ^
              ((exprToString b) ^
                 ("," ^ ((exprToString c) ^ ("," ^ ((exprToString d) ^ ")")))))));;
