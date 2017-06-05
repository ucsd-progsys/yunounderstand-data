
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | TimesThree of expr* expr* expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec exprToString e =
  match e with
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine e -> "sin(pi*" ^ ((exprToString e) ^ ")")
  | Cosine e -> "cos(pi*" ^ ((exprToString e) ^ ")")
  | TimesThree e ->
      (exprToString e) ^
        ("*" ^ ((exprToString f) ^ ("*" ^ (exprToString f))))
  | Average (e,f) ->
      "((" ^ ((exprToString e) ^ ("+" ^ ((exprToString f) ^ ")/2)")))
  | Times (e,f) -> (exprToString e) ^ ("*" ^ (exprToString f))
  | Thresh (e,f,g,h) ->
      "(" ^
        ((exprToString e) ^
           ("<" ^
              ((exprToString f) ^
                 ("?" ^ ((exprToString g) ^ (":" ^ ((exprToString h) ^ ")")))))));;
