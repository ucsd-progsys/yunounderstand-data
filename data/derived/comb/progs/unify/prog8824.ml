
let pi = 4.0 *. (atan 1.0);;

type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Cube of expr
  | Average of expr* expr
  | Times of expr* expr
  | ThreeAverage of expr* expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec eval (e,x,y) =
  match e with
  | VarX  -> x
  | VarY  -> y
  | Sine e' -> sin (pi *. (eval (e', x, y)))
  | Cosine e' -> cos (pi *. (eval (e', x, y)))
  | Cube e' -> (eval (e', x, y)) ** 3.0
  | Average (e1,e2) -> ((eval (e1, x, y)) +. (eval (e2, x, y))) /. 2.0
  | Times (e1,e2) -> (eval (e1, x, y)) *. (eval (e2, x, y))
  | ThreeAverage (e1,e2,e3) ->
      (((eval (e1, x, y)) +. (eval (e2, x, y))) +. (eval (e3, x, y))) /. 3.0
  | Thresh (e1,e2,e3,e4) ->
      if (eval (e1, x, y)) < (eval (e2, x, y))
      then eval (e3, x, y)
      else eval (e4, x, y);;

let _ = ((eval (ThreeAverage (VarX, VarY, VarZ))), 0.5, 0.3, 0.5);;