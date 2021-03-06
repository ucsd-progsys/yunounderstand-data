
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Tangent of expr* expr
  | Square2 of expr* expr* expr
  | Thresh of expr* expr* expr* expr;;

let buildAverage (e1,e2) = Average (e1, e2);;

let buildCosine e = Cosine e;;

let buildSine e = Sine e;;

let buildThresh (a,b,a_less,b_less) = Thresh (a, b, a_less, b_less);;

let buildX () = VarX;;

let buildY () = VarY;;

let rec build (rand,depth) =
  match depth with
  | 0 -> if (rand (0, 1)) = 1 then buildX () else buildY ()
  | _ ->
      let r = rand (3, 100) in
      (match r with
       | r when r < 5 ->
           ((buildAverage (build (rand, (depth - 1)))),
             (build (rand, (depth - 1))))
       | r when (r < 10) && (r > 5) ->
           buildThresh
             ((buildCosine (build (rand, (depth - 1)))),
               (buildCosine (build (rand, (depth - 1)))),
               (build (rand, (depth - 1))), (build (rand, (depth - 1))))
       | r when (r < 25) && (r > 20) -> buildSine (build (rand, (depth - 1)))
       | _ -> buildCosine (build (rand, (depth - 1))));;
