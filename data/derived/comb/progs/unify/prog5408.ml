
let rec wwhile (f,b) =
  match f b with | (h1,h2) -> if h2 then wwhile (f, h1) else h1;;

let fixpoint (f,b) =
  wwhile
    ((let f' b' = if (f b) = b then (b, true) else ((f b), false) in f' b),
      b);;
