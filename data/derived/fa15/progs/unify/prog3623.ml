
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
  if depth = 0
  then let r = rand (0, 2) in match r with | 0 -> buildX () | 1 -> buildY ()
  else
    (let r = rand (0, 11) in
     let d = depth - 1 in
     match r with
     | 0 -> buildAverage ((build (rand, d)), (build (rand, d)))
     | 1 -> buildCosine (build (rand, d))
     | 2 -> buildSine (build (rand, d))
     | 3 -> buildTimes ((build (rand, d)), (build (rand, d)))
     | 4 ->
         buildThresh
           ((build (rand, d)), (build (rand, d)), (build (rand, d)),
             (build (rand, d)))
     | 5 -> buildTimes ((build (rand, d)), (build (rand, d)))
     | 6 -> buildSine (build (rand, d))
     | 7 -> buildCosine (build (rand, d))
     | 8 -> buildX (build (rand, d))
     | 9 -> buildSine (build (rand, d))
     | 10 -> buildCosine (build (rand, d)));;