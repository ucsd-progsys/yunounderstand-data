
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Square of expr
  | Volume of expr* expr* expr;;

type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Sqaure of expr
  | Volume of expr* expr* expr;;

let rec exprToString e =
  match e with
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine t -> "sin(pi*" ^ ((exprToString t) ^ ")")
  | Cosine t -> "cos(pi*" ^ ((exprToString t) ^ ")")
  | Average (s,t) ->
      "((" ^ ((exprToString s) ^ ("+" ^ ((exprToString t) ^ ")/2)")))
  | Times (s,t) -> (exprToString s) ^ ("*" ^ (exprToString t))
  | Thresh (s,t,u,v) ->
      "(" ^
        ((exprToString s) ^
           ("<" ^
              ((exprToString t) ^
                 ("?" ^ ((exprToString u) ^ (":" ^ ((exprToString v) ^ ")")))))))
  | Square s -> "(" ^ ((exprToString s) ^ ")^2")
  | Volume (s,t,u) ->
      "Vol(H: " ^
        ((exprToString s) ^
           (", W: " ^
              ((exprToString t) ^ (", L: " ^ ((exprToString u) ^ ")")))));;
