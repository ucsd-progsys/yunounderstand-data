
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
  | VarX  -> "x"
  | VarY  -> "y"
  | Sine n -> "sin(" ^ ((exprToString n) ^ ")")
  | Cosine n -> "cos(" ^ ((exprToString n) ^ ")")
  | Average n ->
      let (x,y) = n in
      "((" ^ ((exprToString x) ^ ("+" ^ ((exprToString y) ^ ")/2)")))
  | Times n ->
      let (x,y) = n in
      "((" ^ ((exprToString x) ^ ("*" ^ ((exprToString y) ^ ")")))
  | Thresh n ->
      let (x,y,z,w) = n in
      "(" ^
        ((exprToString x) ^
           ("<" ^
              ((exprToString y) ^
                 ("?" ^ ((exprToString z) ^ (":" ^ (exprToString w)))))));;
