
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec exprToString e =
  let expr = exprToString in
  match e with
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine t -> "sin(pi*" ^ ((expr t) ^ ")")
  | Cosine t -> "cos(pi*" ^ ((expr t) ^ ")")
  | Average (s,t) -> "((" ^ ((ex s) ^ ("+" ^ ((ex t) ^ ")/2)")))
  | Times (s,t) -> (ex s) ^ ("*" ^ (ex t))
  | Thresh (s,t,u,v) ->
      "(" ^
        ((ex s) ^
           ("<" ^ ((ex t) ^ ("?" ^ ((ex u) ^ (":" ^ ((ex v) ^ ")")))))))
  | FunnyTimes (s,t,u) ->
      "(floor " ^ ((ex s) ^ ("* ceil " ^ ((ex t) ^ ("*" ^ ((ex u) ^ ")")))))
  | Sqr s -> "(" ^ ((ex s) ^ ("*" ^ ((ex s) ^ ")")));;
