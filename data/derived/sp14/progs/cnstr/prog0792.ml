
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
  | Sine ex -> "sin (pi*)" ^ ((exprToString ex) ^ ")")
  | Cosine ex -> "cos (pi*)" ^ ((exprToString ex) ^ ")")
  | Average (ex1,ex2) ->
      "((" ^ ((exprToSring ex1) ^ (" + " ^ ((exprToString ex2) ^ ")/2)")))
  | Times (ex1,ex2) -> (exprToString expr1) ^ (" * " ^ (exprToString expr2))
  | Tresh (ex1,ex2,ex3,ex4) ->
      "(" ^
        ((exprToString expr1) ^
           ("<" ^
              ((exprToString expr2) ^
                 (" ? " ^
                    ((exprToString expr3) ^
                       (" : " ^ ((exprToString expr4) ^ ")")))))));;
