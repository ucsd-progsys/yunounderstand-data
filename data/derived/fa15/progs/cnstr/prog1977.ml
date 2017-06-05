
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Root of expr
  | Average of expr* expr
  | Times of expr* expr
  | Pivot of expr* expr* expr
  | Thresh of expr* expr* expr* expr;;

type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Flip of expr
  | Average of expr* expr
  | Times of expr* expr
  | Pivot of expr* expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec exprToString e =
  match e with
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine x1 -> "sin(pi*" ^ ((exprToString x1) ^ ")")
  | Cosine x2 -> "cos(pi*" ^ ((exprToString x2) ^ ")")
  | Root x3 -> "sqrt(" ^ ((exprToString x3) ^ ")")
  | Average (x4,x5) ->
      "((" ^ ((exprToString x4) ^ ("+" ^ ((exprToString x5) ^ ")/2)")))
  | Times (x6,x7) -> (exprToString x6) ^ ("*" ^ (exprToString x7))
  | Thresh (x8,x9,x10,x11) ->
      "(" ^
        ((exprToString x8) ^
           ("<" ^
              ((exprToString x9) ^
                 ("?" ^
                    ((exprToString x10) ^ (":" ^ ((exprToString x11) ^ ")")))))))
  | Pivot (x12,x13,x14) ->
      "(" ^
        ((exprToString x12) ^
           ("<0?" ^ ((exprToString x13) ^ (":" ^ ((exprToString x14) ^ ")")))));;
