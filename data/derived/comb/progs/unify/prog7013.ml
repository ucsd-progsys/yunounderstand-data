
let rec wwhile (f,b) =
  let res = f b in
  match res with | (x,y) when y = true -> wwhile (f, x) | (x,y) -> x;;

let fixpoint (f,b) =
  let gs x =
    let isFPoint s = (f s) = s in
    let rec go r =
      if (isFPoint r) = true
      then r
      else if (isFPoint r) = false then go (f r) in
    ((go x), (isFPoint x)) in
  wwhile (gs, b);;

let _ =
  let g x = truncate (1e6 *. (cos (1e-6 *. (float x)))) in fixpoint (g, 0);;
