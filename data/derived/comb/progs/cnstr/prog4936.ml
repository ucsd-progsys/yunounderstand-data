
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec exprToString e =
  let a = exprToString in
  match e with
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine t -> "sin(pi*" ^ ((ex t) ^ ")")
  | Cosine t -> "cos(pi*" ^ ((ex t) ^ ")")
  | Average (s,t) -> "((" ^ ((ex s) ^ ("+" ^ ((ex t) ^ ")/2)")))
  | Times (s,t) -> (ex s) ^ ("*" ^ (ex t))
  | Thresh (s,t,u,v) ->
      "(" ^
        ((ex s) ^
           ("<" ^ ((ex t) ^ ("?" ^ ((ex u) ^ (":" ^ ((ex v) ^ ")")))))))
  | Extra (s,t,u) ->
      "sin(pi*" ^
        ((ex s) ^ (") * cos (" ^ ((ex t) ^ (") * sin(" ^ ((ex u) ^ ")")))))
  | Stuff t -> "cos(pi*" ^ ("(sin(pi*" ^ ((ex t) ^ ")))"));;
