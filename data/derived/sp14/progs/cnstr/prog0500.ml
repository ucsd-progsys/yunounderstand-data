
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec ets e s =
  match e with
  | [] -> s
  | VarX  -> ets (e, (s ^ VarX))
  | VarY  -> ets (e, (s ^ VarY))
  | Sine  -> ets (e, (s ^ Sine))
  | Cosine  -> ets (e, (s ^ Cosine))
  | Average  -> ets (e, (s ^ Average))
  | Times  -> ets (e, (s ^ Times))
  | Thresh  -> ets (e, (s ^ Thresh));;
