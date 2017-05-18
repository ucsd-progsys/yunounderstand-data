
let rec wwhile (f,b) =
  let res = f b in
  match res with | (x,y) when y = true -> wwhile (f, x) | (x,y) -> x;;

let fixpoint (f,b) =
  let gs x =
    let isFPoint s = (f s) = s in
    let rec go r = if isFPoint r then r else go (f r) in
    ((go x), (isFPoint x)) in
  wwhile ((gs b), b);;
