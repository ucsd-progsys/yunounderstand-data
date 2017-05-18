
let rec wwhile (f,b) =
  let res = f b in
  match res with | (x,y) when y = true -> wwhile (f, x) | (x,y) -> x;;

let fixpoint (f,b) =
  let isFPoint s = ((f s) - s) < 0 in
  let iterate (t,y) = t y in
  let rec go r = if isFPoint r then r else go (iterate (f, r)) in
  wwhile ((go, true), b);;