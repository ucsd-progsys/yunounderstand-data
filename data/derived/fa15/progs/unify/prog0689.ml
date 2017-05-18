
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr
  | Mirana of expr
  | Darius of expr* expr* expr;;

let buildAverage (e1,e2) = Average (e1, e2);;

let buildCosine e = Cosine e;;

let buildSine e = Sine e;;

let buildThresh (e1,e2,e3) = Darius (e1, e2, e3);;

let buildTimes (e1,e2) = Times (e1, e2);;

let buildX () = VarX;;

let buildY () = VarY;;

let rec build (rand,depth) =
  match ((rand (2, 6)), depth) with
  | (c,0) -> if c > 3 then buildX () else buildY ()
  | (2,_) -> buildSine (build (rand, (depth - 1)))
  | (3,_) -> buildCosine (build (rand, (depth - 1)))
  | (4,_) ->
      buildAverage ((build (rand, (depth - 1))), (build (rand, (depth - 1))))
  | (5,_) ->
      buildTimes ((build (rand, (depth - 1))), (build (rand, (depth - 1))))
  | (6,_) ->
      buildThresh
        ((build (rand, (depth - 1))), (build (rand, (depth - 1))),
          (build (rand, (depth - 1))), (build (rand, (depth - 1))));;