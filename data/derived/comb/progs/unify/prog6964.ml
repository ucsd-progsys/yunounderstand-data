
let rec wwhile (f,b) =
  let res = f b in
  match res with | (x,y) when y = true -> wwhile (f, x) | (x,y) -> x;;

let fixpoint (f,b) =
  let funt x = let xx = f f (f (f b)) in (xx, ((f b) = b)) in
  wwhile (funt, (f b));;
