
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
  | Sine m -> "sin(" ^ ((exprToString m) ^ ")")
  | Cosine m -> "cos(" ^ ((exprToString m) ^ ")")
  | Average (m,n) ->
      "((" ^ ((exprToString m) ^ ("+" ^ ((exprToString n) ^ ")/2)")))
  | Times (m,n) -> (exprToString m) ^ ("*" ^ (exprToString n))
  | Tresh (m,n,o,p) ->
      "(" ^
        ((exprToString m) ^
           ("<" ^
              ((exprToString n) ^
                 ("?" ^ ((exprToString o) ^ (":" ^ (exprToString p)))))));;
