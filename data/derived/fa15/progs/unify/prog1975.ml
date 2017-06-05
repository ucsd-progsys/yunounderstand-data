
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

let rec build (rand,depth) =
  let r = if depth > 0 then rand (2, 11) else rand (0, 2) in
  match r with
  | 0 -> buildX ()
  | 1 -> buildY ()
  | 2..4 -> buildSine (build (rand, (depth - 1)))
  | 5..7 -> buildCosine (build (rand, (depth - 1)))
  | 8 ->
      buildAverage ((build (rand, (depth - 1))), (build (rand, (depth - 1))))
  | 9 ->
      buildTimes ((build (rand, (depth - 1))), (build (rand, (depth - 1))))
  | 10 ->
      buildThresh
        ((build (rand, (depth - 1))), (build (rand, (depth - 1))),
          (build (rand, (depth - 1))), (build (rand, (depth - 1))));;
