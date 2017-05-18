
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Circ of expr* expr
  | Oscillate of expr;;

let rec exprToString e =
  match e with
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine sine -> "sin(pi*" ^ ((exprToString sine) ^ ")")
  | Cosine cosine -> "cos(pi*" ^ ((exprToString cosine) ^ ")")
  | Average (e1,e2) ->
      "((" ^ ((exprToString e1) ^ ("+" ^ ((exprToString e2) ^ ")/2)")))
  | Times (t1,t2) -> (exprToString t1) ^ ("*" ^ (exprToString t2))
  | Thresh (th1,th2,th3,th4) ->
      "(" ^
        ((exprToString th1) ^
           ("<" ^
              ((exprToString th2) ^
                 ("?" ^
                    ((exprToString th3) ^ (":" ^ ((exprToString th4) ^ ")")))))))
  | Circ circ1 -> "sqrt(|1-" ^ ((exprToString circ1) ^ "^2|)")
  | Oscillate m4 ->
      "(" ^
        ((exprToString m4) ^
           ("/((1-" ^
              ((exprToString m4) ^ (")^2+" ^ ((exprToString m4) ^ "^2))")))));;
