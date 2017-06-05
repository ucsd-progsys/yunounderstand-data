
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let buildAverage (e1,e2) = Average (e1, e2);;

let buildCosine e = Cosine e;;

let buildSine e = Sine e;;

let buildThresh (a,b,a_less,b_less) = Thresh (a, b, a_less, b_less);;

let buildTimes (e1,e2) = Times (e1, e2);;

let buildX () = VarX;;

let buildY () = VarY;;

let rec eval (e,x,y) =
  match e with
  | VarX  -> buildX ()
  | VarY  -> buildY ()
  | Sine e -> buildSine e
  | Cosine e -> buildCosine e
  | Average (e1,e2) -> buildAverage (e1, e2)
  | Times (e1,e2) -> buildTimes (e1, e2)
  | Thresh (a,b,a_less,b_less) -> buildThresh (a, b, a_less, b_less);;

let eval_fn e (x,y) =
  let rv = eval (e, x, y) in assert (((-1.0) <= rv) && (rv <= 1.0)); rv;;
