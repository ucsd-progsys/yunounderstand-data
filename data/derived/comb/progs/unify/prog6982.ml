
let rec wwhile (f,b) =
  let res = f b in
  match res with | (x,y) when y = true -> wwhile (f, x) | (x,y) -> x;;

let fixpoint (f,b) =
  let isFPoint = ((f b) - b) < 0 in
  let rec test x = if isFPoint x then (x, true) else ((test x), false) in
  wwhile (isFPoint, b);;
