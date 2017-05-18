
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec eval (e,x,y) =
  match e with
  | VarX x' -> printf "%s" x
  | VarY y' -> printf "%s" y
  | Sine sin -> printf "sin(%s)" sin
  | Cosine cos -> printf "cos(%s)" cos
  | Average ave -> printf "((%s+%s)/2)" ave
  | Times t -> printf "%s*%s" t
  | Thresh th -> printf "(%s<*%s?%s:%s)" th;;
