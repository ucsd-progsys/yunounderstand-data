
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr;;

let rec exprToString e =
  match e with
  | VarX  -> Printf.sprintf "x"
  | VarY  -> Printf.sprintf "y"
  | Sine x -> Printf.sprintf "sin(pi*%s)" x
  | Cosine x -> Printf.sprintf "cos(pi*%s)" x
  | Average (x,y) -> Printf.sprintf "((%s+%s)/2)" x y
  | Times (x,y) -> Printf.sprintf "%s*%s" x y
  | Thresh (x,y,z,a) -> Printf.sprintf "%s<%s?%s:%s" x y z a;;
